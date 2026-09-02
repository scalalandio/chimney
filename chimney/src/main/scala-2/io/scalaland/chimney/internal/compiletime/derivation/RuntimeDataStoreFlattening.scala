package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

private[derivation] object RuntimeDataStoreFlattening {

  sealed trait ChainBase
  case object EmptyBase extends ChainBase
  case class OpaqueBase(tree: Any) extends ChainBase

  /** Analyze the prefix tree and return the chain base (empty or opaque) together with the accumulated data terms.
    *
    * The walk always succeeds:
    *   - `EmptyBase` + data → `RuntimeDataStore.wrap(Array(data))`
    *   - `EmptyBase` + no data → `RuntimeDataStore.empty`
    *   - `OpaqueBase(tree)` + data → `tree.runtimeData.prependedAll(Array(data))`
    *   - `OpaqueBase(tree)` + no data → `tree.runtimeData` (no optimization possible)
    */
  def analyzeChain(
      prefixTree: blackbox.Context#Tree
  )(c: blackbox.Context): (ChainBase, List[c.universe.Tree]) = {
    import c.universe.*

    val updateSymbol = typeOf[runtime.WithRuntimeDataStore.type].decl(TermName("update"))
    val emptyRDSSymbol = typeOf[runtime.RuntimeDataStore.type].decl(TermName("empty"))
    val emptyTransformerOverrides = typeOf[runtime.TransformerOverrides.Empty]
    val emptyPatcherOverrides = typeOf[runtime.PatcherOverrides.Empty]

    val acc = collection.mutable.ListBuffer.empty[Tree]

    def stripCasts(t: Tree): Tree = t match {
      case TypeApply(Select(inner, TermName("asInstanceOf")), _) => stripCasts(inner)
      case Typed(inner, _)                                       => stripCasts(inner)
      case _                                                     => t
    }

    def hasEmptyRDS(args: List[Tree]): Boolean =
      args.exists(a => stripCasts(a).symbol == emptyRDSSymbol)

    def hasEmptyOverrides(t: Tree): Boolean = {
      val tpe = t.tpe
      tpe != null && tpe != NoType && tpe.typeArgs.exists(arg =>
        arg =:= emptyTransformerOverrides || arg =:= emptyPatcherOverrides
      )
    }

    def walk(t: Tree): ChainBase = {
      val core = stripCasts(t)
      core match {
        // WithRuntimeDataStore.update(inner, data) — collect data and recurse
        case Apply(fn, List(inner, data)) if fn.symbol == updateSymbol =>
          acc += data; walk(inner)
        case Apply(TypeApply(fn, _), List(inner, data)) if fn.symbol == updateSymbol =>
          acc += data; walk(inner)

        // Method on New(single arg): flag wrappers (new OfPTI(inner).enableX) or
        // extension methods (new TransformerOps(source).into[To]).
        // If the expression itself has Empty overrides, it's a valid chain base (stop).
        // Otherwise recurse through the wrapper to look for data deeper in the chain.
        case expr @ Select(Apply(Select(New(_), _), List(inner)), _) =>
          if (hasEmptyOverrides(expr)) EmptyBase else walk(inner)
        case expr @ TypeApply(Select(Apply(Select(New(_), _), List(inner)), _), _) =>
          if (hasEmptyOverrides(expr)) EmptyBase else walk(inner)
        case expr @ Apply(TypeApply(Select(Apply(Select(New(_), _), List(inner)), _), _), _) =>
          if (hasEmptyOverrides(expr)) EmptyBase else walk(inner)
        case expr @ Apply(Select(Apply(Select(New(_), _), List(inner)), _), _) =>
          if (hasEmptyOverrides(expr)) EmptyBase else walk(inner)

        // Direct constructor (e.g. new TransformerInto(source, td))
        case Apply(Select(New(_), _), args) =>
          if (hasEmptyRDS(args)) EmptyBase else OpaqueBase(core)

        // Catch-all: factory methods, val references, etc.
        case other =>
          if (hasEmptyOverrides(other)) EmptyBase else OpaqueBase(other)
      }
    }

    val tree = prefixTree.asInstanceOf[Tree]
    val base = walk(tree)
    (base, acc.toList.asInstanceOf[List[c.universe.Tree]])
  }

  /** For DSL wrappers that carry a source (e.g. TransformerInto, PatcherUsing), extract the constructor arguments from
    * the base of a linear chain. Recurses through `WithRuntimeDataStore.update` calls and intermediate wrappers (flag
    * DSL classes like `OfPartialTransformerInto`, value-class extension methods) to find the direct `New` constructor.
    * Returns `None` when the chain base is not a recognizable constructor.
    */
  def extractBaseConstructorArgs(
      prefixTree: blackbox.Context#Tree
  )(c: blackbox.Context): Option[List[c.universe.Tree]] = {
    import c.universe.*

    val updateSymbol = typeOf[runtime.WithRuntimeDataStore.type].decl(TermName("update"))

    def stripCasts(t: Tree): Tree = t match {
      case TypeApply(Select(inner, TermName("asInstanceOf")), _) => stripCasts(inner)
      case Typed(inner, _)                                       => stripCasts(inner)
      case _                                                     => t
    }

    def walk(t: Tree): Option[List[Tree]] = {
      val core = stripCasts(t)
      core match {
        case Apply(fn, List(inner, _)) if fn.symbol == updateSymbol =>
          walk(inner)
        case Apply(TypeApply(fn, _), List(inner, _)) if fn.symbol == updateSymbol =>
          walk(inner)
        // Method on New(single arg) — recurse through wrappers
        case Select(Apply(Select(New(_), _), List(inner)), _) =>
          walk(inner)
        case TypeApply(Select(Apply(Select(New(_), _), List(inner)), _), _) =>
          walk(inner)
        case Apply(TypeApply(Select(Apply(Select(New(_), _), List(inner)), _), _), _) =>
          walk(inner)
        case Apply(Select(Apply(Select(New(_), _), List(inner)), _), _) =>
          walk(inner)
        // Direct constructor
        case Apply(Select(New(_), _), args) =>
          Some(args)
        case _ =>
          None
      }
    }

    walk(prefixTree.asInstanceOf[Tree]).map(_.asInstanceOf[List[c.universe.Tree]])
  }
}
