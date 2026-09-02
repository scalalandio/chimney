package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.internal.runtime

import scala.quoted.{Expr, Quotes, Type, Varargs}

/** Scala 3-only optimization: detects linear DSL chains and emits a flat `RuntimeDataStore.wrap(Array(...))` instead of
  * evaluating the cons-cell chain at runtime. Falls back to the standard `prefix.runtimeData` when the prefix contains
  * a `val` reference or any tree shape the analysis cannot recognize.
  */
private[derivation] object RuntimeDataStoreFlattening {

  def flattenedRuntimeDataStore[D: Type](
      prefix: Expr[D]
  )(runtimeDataAccess: Expr[D] => Expr[TransformerDefinitionCommons.RuntimeDataStore])(using
      q: Quotes
  ): Expr[TransformerDefinitionCommons.RuntimeDataStore] = {
    import q.reflect.*
    extractChainData(prefix.asTerm) match {
      case Some(dataTerms) if dataTerms.nonEmpty =>
        val dataExprs: Seq[Expr[Any]] = dataTerms.map(_.asExprOf[Any])
        val seqExpr: Expr[Seq[Any]] = Varargs(dataExprs)
        '{ runtime.RuntimeDataStore.wrap($seqExpr.toArray[Any]) }
      case Some(_) =>
        '{ runtime.RuntimeDataStore.empty }
      case None =>
        runtimeDataAccess(prefix)
    }
  }

  private def extractChainData(prefixTerm: Any)(using q: Quotes): Option[List[q.reflect.Term]] = {
    import q.reflect.*

    val updateMethodSymbol: Symbol = {
      val wrdsModule = Symbol.requiredModule("io.scalaland.chimney.internal.runtime.WithRuntimeDataStore")
      wrdsModule.methodMember("update").head
    }

    val rdsModuleSymbol: Symbol =
      Symbol.requiredModule("io.scalaland.chimney.internal.runtime.RuntimeDataStore")

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

    def walk(t: Term): Boolean = {
      val core = stripCasts(stripWrappers(t))
      core match {
        case Apply(fn, List(inner, data)) if fn.symbol == updateMethodSymbol =>
          acc += data
          walk(inner)
        case Apply(TypeApply(fn, _), List(inner, data)) if fn.symbol == updateMethodSymbol =>
          acc += data
          walk(inner)
        case Apply(Select(New(_), "<init>"), args)               => hasEmptyRDS(args)
        case Apply(TypeApply(Select(New(_), "<init>"), _), args) => hasEmptyRDS(args)
        case _                                                   => false
      }
    }

    val term = prefixTerm.asInstanceOf[Term]
    if walk(term) then Some(acc.toList)
    else None
  }
}
