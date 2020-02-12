package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.whitebox

class TransformerIntoWhiteboxMacros(val c: whitebox.Context) extends DslWhiteboxMacros with MacroUtils {

  import c.universe._

  def withFieldConstImpl[
      F[_]: WTTF,
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](
      selector: c.Tree,
      value: c.Tree
  ): c.Tree = {
    val fn = TermName(c.freshName("ti"))
    q"""
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.dsl.TransformerInto($fn.source, $fn.td.withFieldConst($selector, $value))
     """
  }

  def withFieldConstImplF[
      F[_]: WTTF,
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](
      selector: c.Tree,
      value: c.Tree
  ): c.Tree = {
    val fn = TermName(c.freshName("ti"))
    q"""
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.dsl.TransformerInto($fn.source, $fn.td.withFieldConstF($selector, $value))
     """
  }

  def withFieldComputedImpl[
      F[_]: WTTF,
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](selector: c.Tree, map: c.Tree): c.Tree = {
    val fn = TermName(c.freshName("ti"))
    q"""
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.dsl.TransformerInto($fn.source, $fn.td.withFieldComputed($selector, $map))
     """
  }

  def withFieldComputedImplF[
      F[_]: WTTF,
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](selector: c.Tree, map: c.Tree): c.Tree = {
    val fn = TermName(c.freshName("ti"))
    q"""
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.dsl.TransformerInto($fn.source, $fn.td.withFieldComputedF($selector, $map))
     """
  }

  def withFieldRenamedImpl[
      F[_]: WTTF,
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](selectorFrom: c.Tree, selectorTo: c.Tree): c.Tree = {
    val fn = TermName(c.freshName("ti"))
    q"""
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.dsl.TransformerInto($fn.source, $fn.td.withFieldRenamed($selectorFrom, $selectorTo))
     """
  }

  def withCoproductInstanceImpl[
      F[_]: WTTF,
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Inst: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](
      f: c.Tree
  ): c.Tree = {
    val fn = TermName(c.freshName("ti"))
    q"""
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.dsl.TransformerInto($fn.source, $fn.td.withCoproductInstance($f))
     """
  }

}
