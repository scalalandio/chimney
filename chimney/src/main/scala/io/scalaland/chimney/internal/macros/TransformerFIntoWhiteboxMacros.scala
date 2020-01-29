package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.whitebox

class TransformerFIntoWhiteboxMacros(val c: whitebox.Context) extends MacroUtils {

  import c.universe._

  def withFieldConstImpl(selector: c.Tree, value: c.Tree): c.Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldConst($selector, $value))"
  }

  def withFieldConstFImpl(selector: c.Tree, value: c.Tree): c.Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldConstF($selector, $value))"
  }

  def withFieldComputedImpl(selector: c.Tree, map: c.Tree): c.Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldComputed($selector, $map))"
  }

  def withFieldComputedFImpl(selector: c.Tree, map: c.Tree): c.Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldComputedF($selector, $map))"
  }

  def withFieldRenamedImpl(selectorFrom: c.Tree, selectorTo: c.Tree): c.Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldRenamed($selectorFrom, $selectorTo))"
  }

  def withCoproductInstanceImpl(f: c.Tree): c.Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withCoproductInstance($f))"
  }

  def withCoproductInstanceFImpl(f: c.Tree): c.Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withCoproductInstanceF($f))"
  }

}
