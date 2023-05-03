package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros

import scala.quoted.*

object PartialTransformerIntoImpl {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      tiExpr: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      valueExpr: Expr[U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    PartialTransformerDefinitionImpl.withFieldConstImpl('{ ${ tiExpr }.td }, selectorExpr, valueExpr) match {
      case '{ $td: PartialTransformerDefinition[From, To, cfg, Flags] } =>
        '{ new PartialTransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
    }
  }

  def withFieldConstPartialImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      tiExpr: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      valueExpr: Expr[partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    PartialTransformerDefinitionImpl.withFieldConstPartialImpl('{ ${ tiExpr }.td }, selectorExpr, valueExpr) match {
      case '{ $td: PartialTransformerDefinition[From, To, cfg, Flags] } =>
        '{ new PartialTransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
    }
  }

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      tiExpr: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      fExpr: Expr[From => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    PartialTransformerDefinitionImpl.withFieldComputedImpl('{ ${ tiExpr }.td }, selectorExpr, fExpr) match {
      case '{ $td: PartialTransformerDefinition[From, To, cfg, Flags] } =>
        '{ new PartialTransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
    }
  }

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      tiExpr: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      fExpr: Expr[From => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    PartialTransformerDefinitionImpl.withFieldComputedPartialImpl('{ ${ tiExpr }.td }, selectorExpr, fExpr) match {
      case '{ $td: PartialTransformerDefinition[From, To, cfg, Flags] } =>
        '{ new PartialTransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
    }
  }

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      tiExpr: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selectorFromExpr: Expr[From => T],
      selectorToExpr: Expr[To => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    PartialTransformerDefinitionImpl.withFieldRenamed('{ ${ tiExpr }.td }, selectorFromExpr, selectorToExpr) match {
      case '{ $td: PartialTransformerDefinition[From, To, cfg, Flags] } =>
        '{ new PartialTransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
    }
  }

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      tiExpr: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      fExpr: Expr[Inst => To]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    PartialTransformerDefinitionImpl.withCoproductInstance('{ ${ tiExpr }.td }, fExpr) match {
      case '{ $td: PartialTransformerDefinition[From, To, cfg, Flags] } =>
        '{ new PartialTransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
    }
  }

  def withCoproductInstancePartialImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      tiExpr: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      fExpr: Expr[Inst => partial.Result[To]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    PartialTransformerDefinitionImpl.withCoproductInstancePartial('{ ${ tiExpr }.td }, fExpr) match {
      case '{ $td: PartialTransformerDefinition[From, To, cfg, Flags] } =>
        '{ new PartialTransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
    }
  }

  def transform[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      ImplicitScopeFlags <: TransformerFlags: Type
  ](
      source: Expr[From],
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      failFast: Boolean
  )(using Quotes): Expr[partial.Result[To]] = {
    TransformerMacros.derivePartialTransformerResultWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](
      source,
      td,
      failFast
    )
  }
}
