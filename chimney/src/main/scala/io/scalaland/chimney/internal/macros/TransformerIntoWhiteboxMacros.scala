package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.whitebox

class TransformerIntoWhiteboxMacros(val c: whitebox.Context) extends MacroUtils {

  import c.universe._

  def withFieldConstImpl(selector: c.Tree, value: c.Tree): c.Tree = {
    // when value inlined, it crashes the compiler in 2.11/2.12
    val v = TermName(c.freshName("v"))
    q"""
      val $v = $value
      ${c.prefix.tree}.__refineTransformerDefinition(_.withFieldConst($selector, $v))
    """
  }

  def withFieldConstFImpl[F[+_]](
      selector: c.Tree,
      value: c.Tree
  )(
      implicit F: c.WeakTypeTag[F[_]]
  ): c.Tree = {
    q"${c.prefix.tree}.lift[$F].withFieldConstF($selector, $value)"
  }

  def withFieldComputedImpl(selector: c.Tree, map: c.Tree): c.Tree = {
    // when map inlined, it crashes the compiler on 2.12/2.13
    val f = TermName(c.freshName("f"))
    q"""
      val $f = $map
      ${c.prefix.tree}.__refineTransformerDefinition(_.withFieldComputed($selector, $f))
    """
  }

  def withFieldComputedFImpl[F[+_]](
      selector: c.Tree,
      map: c.Tree
  )(
      implicit F: c.WeakTypeTag[F[_]]
  ): c.Tree = {
    q"${c.prefix.tree}.lift[$F].withFieldComputedF($selector, $map)"
  }

  def withFieldRenamedImpl(selectorFrom: c.Tree, selectorTo: c.Tree): c.Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldRenamed($selectorFrom, $selectorTo))"
  }

  def withCoproductInstanceImpl(f: c.Tree): c.Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withCoproductInstance($f))"
  }

  def withCoproductInstanceFImpl[F[+_]](f: c.Tree)(implicit F: c.WeakTypeTag[F[_]]): c.Tree = {
    q"${c.prefix.tree}.lift[$F].withCoproductInstanceF($f)"
  }
}
