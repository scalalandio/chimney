package io.scalaland.chimney.internal.macros.dsl

import io.scalaland.chimney.internal.utils.DslMacroUtils

import scala.annotation.unused
import scala.reflect.macros.whitebox

class TransformerDefinitionWhiteboxMacros(val c: whitebox.Context) extends DslMacroUtils {

  import CfgTpes._
  import c.universe._

  def withFieldConstImpl[C: WeakTypeTag](selector: Tree, value: Tree)(@unused ev: Tree): Tree = {
    c.prefix.tree.overrideField[C](selector.extractSelectorFieldName, value, fieldConstT)
  }

  def withFieldConstFImpl[F[+_]](selector: Tree, value: Tree)(@unused ev: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix}.lift[$F].withFieldConstF($selector, $value)"
  }

  def withFieldComputedImpl[C: WeakTypeTag](selector: Tree, f: Tree)(@unused ev: Tree): Tree = {
    c.prefix.tree.overrideField[C](selector.extractSelectorFieldName, f, fieldComputedT)
  }

  def withFieldComputedFImpl[F[+_]](selector: Tree, f: Tree)(@unused ev: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix}.lift[$F].withFieldComputedF($selector, $f)"
  }

  def withFieldRenamedImpl[C: WeakTypeTag](selectorFrom: Tree, selectorTo: Tree): Tree = {
    val (fieldNameFrom, fieldNameTo) = (selectorFrom, selectorTo).extractSelectorsOrAbort
    c.prefix.tree.renameField[C](fieldNameFrom, fieldNameTo)
  }

  def withCoproductInstanceImpl[To: WeakTypeTag, Inst: WeakTypeTag, C: WeakTypeTag](f: Tree): Tree = {
    c.prefix.tree.overrideCoproductInstance[C](weakTypeOf[Inst], weakTypeOf[To], f, coproductInstanceT)
  }

  def withCoproductInstanceFImpl[F[+_]](f: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix}.lift[$F].withCoproductInstanceF($f)"
  }

}
