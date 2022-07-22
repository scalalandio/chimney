package io.scalaland.chimney.internal.macros.dsl

import io.scalaland.chimney.internal.utils.DslMacroUtils

import scala.annotation.unused
import scala.reflect.macros.whitebox

class TransformerIntoWhiteboxMacros(val c: whitebox.Context) extends DslMacroUtils {

  import c.universe._

  def withFieldConstImpl(selector: Tree, value: Tree)(@unused ev: Tree): Tree = {
    c.prefix.tree.refineTransformerDefinition_Hack(
      trees => q"_.withFieldConst($selector, ${trees("value")})",
      "value" -> value
    )
  }

  def withFieldConstFImpl[F[+_]](
      selector: Tree,
      value: Tree
  )(@unused ev: Tree)(
      implicit F: WeakTypeTag[F[_]]
  ): Tree = {
    q"${c.prefix.tree}.lift[$F].withFieldConstF($selector, $value)"
  }

  def withFieldComputedImpl(selector: Tree, f: Tree)(@unused ev: Tree): Tree = {
    c.prefix.tree.refineTransformerDefinition_Hack(
      trees => q"_.withFieldComputed($selector, ${trees("f")})",
      "f" -> f
    )
  }

  def withFieldComputedFImpl[F[+_]](
      selector: Tree,
      f: Tree
  )(@unused ev: Tree)(
      implicit F: WeakTypeTag[F[_]]
  ): Tree = {
    q"${c.prefix.tree}.lift[$F].withFieldComputedF($selector, $f)"
  }

  def withFieldRenamedImpl(selectorFrom: Tree, selectorTo: Tree): Tree = {
    c.prefix.tree.refineTransformerDefinition(q"_.withFieldRenamed($selectorFrom, $selectorTo)")
  }

  def withCoproductInstanceImpl(f: Tree): Tree = {
    c.prefix.tree.refineTransformerDefinition_Hack(
      trees => q"_.withCoproductInstance(${trees("f")})",
      "f" -> f
    )
  }

  def withCoproductInstanceFImpl[F[+_]](f: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix.tree}.lift[$F].withCoproductInstanceF($f)"
  }
}
