package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.internal.runtime

import scala.quoted.{Expr, Quotes, Type, Varargs}

private[derivation] object RuntimeDataStoreFlattening {

  sealed trait ChainBase
  case object EmptyBase extends ChainBase
  case class OpaqueBase(term: Any) extends ChainBase

  /** Analyze the prefix tree and return the chain base together with accumulated data terms.
    *
    * The walk always succeeds:
    *   - `EmptyBase` + data → `RuntimeDataStore.wrap(Array(data))`
    *   - `EmptyBase` + no data → `RuntimeDataStore.empty`
    *   - `OpaqueBase(term)` + data → partial chain, caller uses `prependedAll`
    *   - `OpaqueBase(term)` + no data → no optimization, use `runtimeDataAccess` on original prefix
    */
  def analyzeChain(prefixTerm: Any)(using q: Quotes): (ChainBase, List[q.reflect.Term]) = {
    import q.reflect.*

    val updateMethodSymbol: Symbol = {
      val wrdsModule = Symbol.requiredModule("io.scalaland.chimney.internal.runtime.WithRuntimeDataStore")
      wrdsModule.methodMember("update").head
    }

    val rdsModuleSymbol: Symbol =
      Symbol.requiredModule("io.scalaland.chimney.internal.runtime.RuntimeDataStore")

    val emptyTransformerOverrides: TypeRepr = TypeRepr.of[runtime.TransformerOverrides.Empty]
    val emptyPatcherOverrides: TypeRepr = TypeRepr.of[runtime.PatcherOverrides.Empty]

    val acc = collection.mutable.ListBuffer.empty[Term]

    def stripWrappers(t: Term): Term = t match {
      case Inlined(_, _, body) => stripWrappers(body)
      case Block(Nil, expr)    => stripWrappers(expr)
      case Typed(expr, _)      => stripWrappers(expr)
      case _                   => t
    }

    def stripCasts(t: Term): Term = stripWrappers(t) match {
      case TypeApply(Select(inner, "asInstanceOf"), _) => stripCasts(stripWrappers(inner))
      case other                                       => other
    }

    def isEmptyRDS(t: Term): Boolean = stripWrappers(t) match {
      case Select(qualifier, "empty") => qualifier.symbol == rdsModuleSymbol
      case _                          => false
    }

    def hasEmptyRDS(args: List[Term]): Boolean = args.exists(isEmptyRDS)

    def hasEmptyOverrides(t: Term): Boolean = {
      val tpe = t.tpe
      tpe != TypeRepr.of[Nothing] && tpe.typeArgs.exists(arg =>
        arg =:= emptyTransformerOverrides || arg =:= emptyPatcherOverrides
      )
    }

    def walk(t: Term): ChainBase = {
      val core = stripCasts(stripWrappers(t))
      core match {
        // WithRuntimeDataStore.update(inner, data)
        case Apply(fn, List(inner, data)) if fn.symbol == updateMethodSymbol =>
          acc += data; walk(inner)
        case Apply(TypeApply(fn, _), List(inner, data)) if fn.symbol == updateMethodSymbol =>
          acc += data; walk(inner)

        // Method on New(single arg): flag wrappers or extension methods.
        // Check hasEmptyOverrides on the expression first — if true, it's a valid base.
        // Otherwise recurse through the wrapper.
        case expr @ Select(Apply(Select(New(_), "<init>"), List(inner)), _) =>
          if hasEmptyOverrides(expr) then EmptyBase else walk(inner)
        case expr @ TypeApply(Select(Apply(Select(New(_), "<init>"), List(inner)), _), _) =>
          if hasEmptyOverrides(expr) then EmptyBase else walk(inner)
        case expr @ Apply(TypeApply(Select(Apply(Select(New(_), "<init>"), List(inner)), _), _), _) =>
          if hasEmptyOverrides(expr) then EmptyBase else walk(inner)
        case expr @ Apply(Select(Apply(Select(New(_), "<init>"), List(inner)), _), _) =>
          if hasEmptyOverrides(expr) then EmptyBase else walk(inner)

        // Direct constructor
        case Apply(Select(New(_), "<init>"), args) => if hasEmptyRDS(args) then EmptyBase else OpaqueBase(core)
        case Apply(TypeApply(Select(New(_), "<init>"), _), args) =>
          if hasEmptyRDS(args) then EmptyBase else OpaqueBase(core)

        // Catch-all
        case other => if hasEmptyOverrides(other) then EmptyBase else OpaqueBase(other)
      }
    }

    val term = prefixTerm.asInstanceOf[Term]
    val base = walk(term)
    (base, acc.toList)
  }

  def flattenedRuntimeDataStore[D: Type](
      prefix: Expr[D]
  )(runtimeDataAccess: Expr[D] => Expr[TransformerDefinitionCommons.RuntimeDataStore])(using
      q: Quotes
  ): Expr[TransformerDefinitionCommons.RuntimeDataStore] = {
    import q.reflect.*

    val (base, dataTerms) = analyzeChain(prefix.asTerm)

    base match {
      case EmptyBase if dataTerms.nonEmpty =>
        val dataExprs: Seq[Expr[Any]] = dataTerms.map(_.asExprOf[Any])
        val seqExpr: Expr[Seq[Any]] = Varargs(dataExprs)
        '{ runtime.RuntimeDataStore.wrap($seqExpr.toArray[Any]) }
      case EmptyBase =>
        '{ runtime.RuntimeDataStore.empty }
      case OpaqueBase(baseTerm) if dataTerms.nonEmpty =>
        val bt = baseTerm.asInstanceOf[Term]
        val rdsAccess = Select.unique(bt, "runtimeData").asExprOf[TransformerDefinitionCommons.RuntimeDataStore]
        val dataExprs: Seq[Expr[Any]] = dataTerms.map(_.asExprOf[Any])
        val seqExpr: Expr[Seq[Any]] = Varargs(dataExprs)
        '{ $rdsAccess.prependedAll($seqExpr.toArray[Any]) }
      case _ =>
        runtimeDataAccess(prefix)
    }
  }
}
