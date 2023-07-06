package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.compiletime.dsl.utils.DslMacroUtils
import scala.annotation.unused
import scala.reflect.macros.whitebox

class TransformerDefinitionMacros(val c: whitebox.Context) extends DslMacroUtils {

  import CfgTpes.*
  import c.universe.*

  def withFieldConstImpl[C: WeakTypeTag](selector: Tree, value: Tree)(@unused ev: Tree): Tree =
    c.prefix.tree.overrideField[C](selector.extractSelectorFieldName, value, fieldConstT)

  def withFieldComputedImpl[C: WeakTypeTag](selector: Tree, f: Tree)(@unused ev: Tree): Tree =
    c.prefix.tree.overrideField[C](selector.extractSelectorFieldName, f, fieldComputedT)

  def withFieldRenamedImpl[C: WeakTypeTag](selectorFrom: Tree, selectorTo: Tree): Tree = {
    val (fieldNameFrom, fieldNameTo) = (selectorFrom, selectorTo).extractSelectorsOrAbort
    c.prefix.tree.renameField[C](fieldNameFrom, fieldNameTo)
  }

  def withCoproductInstanceImpl[To: WeakTypeTag, Inst: WeakTypeTag, C: WeakTypeTag](f: Tree): Tree =
    c.prefix.tree.overrideCoproductInstance[C](weakTypeOf[Inst], weakTypeOf[To], f, coproductInstanceT)
}
