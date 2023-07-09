package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.annotation.unused
import scala.reflect.macros.whitebox

final class PartialTransformerDefinitionMacros(val c: whitebox.Context)
    extends PartialTransformerDefinitionGateway
    with DslDefinitionsPlatform {

  private def ptd[From, To, Cfg <: runtime.TransformerCfg, Flags <: runtime.TransformerFlags] =
    c.prefix.asInstanceOf[Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]]]

  // While Scala 2.13 would accept Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]]
  // as return type, Scala 2.12 forces us to return c.Tree in whitebox macros.

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](selector: Expr[To => T], value: Expr[U])(@unused ev: Expr[U <:< T]): c.Tree =
    withFieldConst[From, To, Cfg, Flags, T](ptd, selector, value.upcastExpr[T]).tree

  def withFieldConstPartialImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](selector: Expr[To => T], value: Expr[partial.Result[U]])(@unused ev: Expr[U <:< T]): c.Tree =
    withFieldConstPartial[From, To, Cfg, Flags, T](ptd, selector, value.upcastExpr[partial.Result[T]]).tree

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](selector: Expr[To => T], f: Expr[From => U])(
      @unused ev: Expr[U <:< T]
  ): c.Tree =
    withFieldComputed[From, To, Cfg, Flags, T](ptd, selector, f.upcastExpr[From => T]).tree

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](selector: Expr[To => T], f: Expr[From => partial.Result[U]])(@unused ev: Expr[U <:< T]): c.Tree =
    withFieldComputedPartial[From, To, Cfg, Flags, T](ptd, selector, f.upcastExpr[From => partial.Result[T]]).tree

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](selectorFrom: Expr[From => T], selectorTo: Expr[To => U]): c.Tree =
    withFieldRenamed[From, To, Cfg, Flags, T, U](ptd, selectorFrom, selectorTo).tree

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Inst: Type
  ](f: Expr[Inst => To]): c.Tree =
    withCoproductInstance[From, To, Cfg, Flags, Inst](ptd, f).tree

  def withCoproductInstancePartialImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Inst: Type
  ](f: Expr[Inst => partial.Result[To]]): c.Tree =
    withCoproductInstancePartial[From, To, Cfg, Flags, Inst](ptd, f).tree
}
