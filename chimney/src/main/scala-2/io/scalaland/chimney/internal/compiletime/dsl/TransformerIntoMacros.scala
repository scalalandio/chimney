package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime

import scala.annotation.unused
import scala.reflect.macros.whitebox

final class TransformerIntoMacros(val c: whitebox.Context) extends TransformIntoGateway with DslDefinitionsPlatform {

  private def ti[From, To, Cfg <: runtime.TransformerCfg, Flags <: runtime.TransformerFlags] =
    c.prefix.asInstanceOf[Expr[dsls.TransformerInto[From, To, Cfg, Flags]]]

  // While Scala 2.13 would accept Expr[dsls.TransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]]
  // as return type, Scala 2.12 forces us to return c.Tree in whitebox macros.

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](selector: Expr[To => T], value: Expr[U])(@unused ev: Expr[U <:< T]): c.Tree =
    withFieldConst[From, To, Cfg, Flags, T](ti, selector, value.upcastExpr[T]).tree

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](selector: Expr[To => T], f: Expr[From => U])(@unused ev: Expr[U <:< T]): c.Tree =
    withFieldComputed[From, To, Cfg, Flags, T](ti, selector, f.upcastExpr[From => T]).tree

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      T: Type,
      U: Type
  ](selectorFrom: Expr[From => T], selectorTo: Expr[To => U]): c.Tree =
    withFieldRenamed[From, To, Cfg, Flags, T, U](ti, selectorFrom, selectorTo).tree

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Inst: Type
  ](f: Expr[Inst => To]): c.Tree =
    withCoproductInstance[From, To, Cfg, Flags, Inst](ti, f).tree
}
