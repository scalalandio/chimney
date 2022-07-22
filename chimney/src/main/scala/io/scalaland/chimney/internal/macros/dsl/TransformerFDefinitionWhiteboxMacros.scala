package io.scalaland.chimney.internal.macros.dsl

import io.scalaland.chimney.internal.utils.DslMacroUtils

import scala.annotation.unused
import scala.reflect.macros.whitebox

class TransformerFDefinitionWhiteboxMacros(val c: whitebox.Context) extends DslMacroUtils {

  import CfgTpes._
  import c.universe._

  def withFieldConstImpl[C: WeakTypeTag](selector: Tree, value: Tree)(@unused ev: Tree): Tree = {
    c.prefix.tree.overrideField(selector.extractSelectorFieldName, value, fieldConstT, weakTypeOf[C])
  }

  def withFieldConstFImpl[C: WeakTypeTag, F[_]](selector: Tree, value: Tree)(
      @unused ev: Tree
  )(implicit F: WeakTypeTag[F[_]]): Tree = {
    c.prefix.tree.overrideField(selector.extractSelectorFieldName, value, fieldConstFT, weakTypeOf[C])
  }

  def withFieldComputedImpl[C: WeakTypeTag](selector: Tree, f: Tree)(@unused ev: Tree): Tree = {
    c.prefix.tree.overrideField(selector.extractSelectorFieldName, f, fieldComputedT, weakTypeOf[C])
  }

  def withFieldComputedFImpl[C: WeakTypeTag, F[+_]](selector: Tree, f: Tree)(
      @unused ev: Tree
  )(implicit F: WeakTypeTag[F[_]]): Tree = {
    c.prefix.tree.overrideField(selector.extractSelectorFieldName, f, fieldComputedFT, weakTypeOf[C])
  }

  def withFieldRenamedImpl[
      C: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree = {

    val fieldNameFromOpt = selectorFrom.extractSelectorFieldNameOpt
    val fieldNameToOpt = selectorTo.extractSelectorFieldNameOpt

    (fieldNameFromOpt, fieldNameToOpt) match {
      case (Some(fieldNameFrom), Some(fieldNameTo)) =>
        c.prefix.tree.renameField(fieldNameFrom, fieldNameTo, weakTypeOf[C])
      case (Some(_), None) =>
        c.abort(c.enclosingPosition, s"Selector of type ${selectorTo.tpe} is not valid: $selectorTo")
      case (None, Some(_)) =>
        c.abort(c.enclosingPosition, s"Selector of type ${selectorFrom.tpe} is not valid: $selectorFrom")
      case (None, None) =>
        val inv1 = s"Selector of type ${selectorFrom.tpe} is not valid: $selectorFrom"
        val inv2 = s"Selector of type ${selectorTo.tpe} is not valid: $selectorTo"
        c.abort(c.enclosingPosition, s"Invalid selectors:\n$inv1\n$inv2")
    }
  }

  def withCoproductInstanceImpl[
      To: WeakTypeTag,
      Inst: WeakTypeTag,
      C: WeakTypeTag
  ](f: Tree): Tree = {
    c.prefix.tree.overrideCoproductInstance(weakTypeOf[Inst], weakTypeOf[To], f, coproductInstanceT, weakTypeOf[C])
  }

  def withCoproductInstanceFImpl[
      To: WeakTypeTag,
      Inst: WeakTypeTag,
      C: WeakTypeTag
  ](f: Tree): Tree = {
    c.prefix.tree.overrideCoproductInstance(weakTypeOf[Inst], weakTypeOf[To], f, coproductInstanceFT, weakTypeOf[C])
  }

}
