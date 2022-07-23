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

  def withFieldRenamedImpl[C: WeakTypeTag](selectorFrom: Tree, selectorTo: Tree): Tree = {
    val (fieldNameFrom, fieldNameTo) = (selectorFrom, selectorTo).extractSelectorsOrAbort
    c.prefix.tree.renameField(fieldNameFrom, fieldNameTo, weakTypeOf[C])
  }

  def withCoproductInstanceImpl[To: WeakTypeTag, Inst: WeakTypeTag, C: WeakTypeTag](f: Tree): Tree = {
    c.prefix.tree.overrideCoproductInstance(weakTypeOf[Inst], weakTypeOf[To], f, coproductInstanceT, weakTypeOf[C])
  }

  def withCoproductInstanceFImpl[To: WeakTypeTag, Inst: WeakTypeTag, C: WeakTypeTag](f: Tree): Tree = {
    c.prefix.tree.overrideCoproductInstance(weakTypeOf[Inst], weakTypeOf[To], f, coproductInstanceFT, weakTypeOf[C])
  }

}
