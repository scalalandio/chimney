package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.whitebox

class TransformerFIntoWhiteboxMacros(val c: whitebox.Context) extends MacroUtils {

  import c.universe._

  def withFieldConstImpl(selector: Tree, value: Tree): Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldConst($selector, $value))"
  }

  def withFieldConstFImpl(selector: Tree, value: Tree): Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldConstF($selector, $value))"
  }

  def withFieldComputedImpl(selector: Tree, map: Tree): Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldComputed($selector, $map))"
  }

  def withFieldComputedFImpl(selector: Tree, map: Tree): Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldComputedF($selector, $map))"
  }

  def withFieldRenamedImpl(selectorFrom: Tree, selectorTo: Tree): Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withFieldRenamed($selectorFrom, $selectorTo))"
  }

  def withCoproductInstanceImpl(f: Tree): Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withCoproductInstance($f))"
  }

  def withCoproductInstanceFImpl(f: Tree): Tree = {
    q"${c.prefix.tree}.__refineTransformerDefinition(_.withCoproductInstanceF($f))"
  }

}
