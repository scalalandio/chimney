package io.scalaland.chimney.internal.macros.dsl

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.whitebox

class TransformerFIntoWhiteboxMacros(val c: whitebox.Context) extends MacroUtils {

  import c.universe._

  def withFieldConstImpl(selector: Tree, value: Tree): Tree = {
    c.prefix.tree.refineTransformerDefinition_Hack(
      trees => q"_.withFieldConst($selector, ${trees("value")})",
      "value" -> value
    )
  }

  def withFieldConstFImpl(selector: Tree, value: Tree): Tree = {
    c.prefix.tree.refineTransformerDefinition_Hack(
      trees => q"_.withFieldConstF($selector, ${trees("value")})",
      "value" -> value
    )
  }

  def withFieldComputedImpl(selector: Tree, map: Tree): Tree = {
    c.prefix.tree.refineTransformerDefinition_Hack(
      trees => q"_.withFieldComputed($selector, ${trees("map")})",
      "map" -> map
    )
  }

  def withFieldComputedFImpl(selector: Tree, map: Tree): Tree = {
    c.prefix.tree.refineTransformerDefinition_Hack(
      trees => q"_.withFieldComputedF($selector, ${trees("map")})",
      "map" -> map
    )
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

  def withCoproductInstanceFImpl(f: Tree): Tree = {
    c.prefix.tree.refineTransformerDefinition_Hack(
      trees => q"_.withCoproductInstanceF(${trees("f")})",
      "f" -> f
    )
  }

}
