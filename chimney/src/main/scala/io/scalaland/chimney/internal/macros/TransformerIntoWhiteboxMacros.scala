package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.whitebox

class TransformerIntoWhiteboxMacros(val c: whitebox.Context) extends MacroUtils {

  import c.universe._

  def withFieldConstImpl(selector: Tree, value: Tree): Tree = {
    // when value inlined, it crashes the compiler in 2.11/2.12
    val v = TermName(c.freshName("v"))
    q"""
      val $v = $value
      ${c.prefix.tree}.__refineTransformerDefinition(_.withFieldConst($selector, $v))
    """
  }

  def withFieldConstFImpl[F[+_]](
      selector: Tree,
      value: Tree
  )(
      implicit F: WeakTypeTag[F[_]]
  ): Tree = {
    q"${c.prefix.tree}.lift[$F].withFieldConstF($selector, $value)"
  }

  def withFieldComputedImpl(selector: Tree, map: Tree): Tree = {
    // when map inlined, it crashes the compiler on 2.12/2.13
    val f = TermName(c.freshName("f"))
    q"""
      val $f = $map
      ${c.prefix.tree}.__refineTransformerDefinition(_.withFieldComputed($selector, $f))
    """
  }

  def withFieldComputedFImpl[F[+_]](
      selector: Tree,
      map: Tree
  )(
      implicit F: WeakTypeTag[F[_]]
  ): Tree = {
    q"${c.prefix.tree}.lift[$F].withFieldComputedF($selector, $map)"
  }

  def withFieldRenamedImpl(selectorFrom: Tree, selectorTo: Tree): Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldRenamed($selectorFrom, $selectorTo))"
  }

  def withCoproductInstanceImpl(f: Tree): Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withCoproductInstance($f))"
  }

  def withCoproductInstanceFImpl[F[+_]](f: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix.tree}.lift[$F].withCoproductInstanceF($f)"
  }
}
