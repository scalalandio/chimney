package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

private[derivation] object RuntimeDataStoreFlattening {

  /** Analyze the prefix tree for a linear DSL chain and, if found, emit a flat `RuntimeDataStore.wrap(Array(...))`.
    * Returns `None` when the chain cannot be recognized (e.g. it contains a `val` reference).
    */
  def flattenedRuntimeDataStore(
      prefixTree: blackbox.Context#Tree
  )(c: blackbox.Context): Option[c.Expr[TransformerDefinitionCommons.RuntimeDataStore]] = {
    import c.universe.*

    val updateSymbol = typeOf[runtime.WithRuntimeDataStore.type].decl(TermName("update"))
    val emptySymbol = typeOf[runtime.RuntimeDataStore.type].decl(TermName("empty"))

    val acc = collection.mutable.ListBuffer.empty[Tree]

    def stripCasts(t: Tree): Tree = t match {
      case TypeApply(Select(inner, TermName("asInstanceOf")), _) => stripCasts(inner)
      case Typed(inner, _)                                       => stripCasts(inner)
      case _                                                     => t
    }

    def hasEmptyRDS(args: List[Tree]): Boolean = args match {
      case List(arg) => arg.symbol == emptySymbol
      case _         => args.exists(a => stripCasts(a).symbol == emptySymbol)
    }

    def walk(t: Tree): Boolean = {
      val core = stripCasts(t)
      core match {
        case Apply(fn, List(inner, data)) if fn.symbol == updateSymbol =>
          acc += data
          walk(inner)
        case Apply(TypeApply(fn, _), List(inner, data)) if fn.symbol == updateSymbol =>
          acc += data
          walk(inner)
        case Apply(Select(New(_), _), args) => hasEmptyRDS(args)
        case _                              => false
      }
    }

    val tree = prefixTree.asInstanceOf[Tree]
    if (walk(tree)) {
      val dataTerms = acc.toList
      if (dataTerms.nonEmpty) {
        Some(
          c.Expr[TransformerDefinitionCommons.RuntimeDataStore](
            q"_root_.io.scalaland.chimney.internal.runtime.RuntimeDataStore.wrap(_root_.scala.Array[Any](..$dataTerms))"
          )
        )
      } else {
        Some(
          c.Expr[TransformerDefinitionCommons.RuntimeDataStore](
            q"_root_.io.scalaland.chimney.internal.runtime.RuntimeDataStore.empty"
          )
        )
      }
    } else None
  }

  /** For DSL wrappers that carry a source (e.g. TransformerInto, PatcherUsing), extract the constructor arguments from
    * the base of a linear chain. Returns the base constructor argument trees if the chain is linear, `None` otherwise.
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
        case Apply(Select(New(_), _), args) => Some(args)
        case _                                               => None
      }
    }

    walk(prefixTree.asInstanceOf[Tree]).map(_.asInstanceOf[List[c.universe.Tree]])
  }
}
