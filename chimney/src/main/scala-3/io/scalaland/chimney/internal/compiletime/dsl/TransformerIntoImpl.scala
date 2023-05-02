package io.scalaland.chimney.internal.compiletime.dsl

import scala.quoted.*
import io.scalaland.chimney.*
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros

import scala.quoted.*

object TransformerIntoImpl {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      tiExpr: Expr[TransformerInto[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      valueExpr: Expr[U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    TransformerDefinitionImpl.withFieldConstImpl('{ ${ tiExpr }.td }, selectorExpr, valueExpr) match {
      case '{ $td: TransformerDefinition[From, To, cfg, Flags] } =>
        '{ new TransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
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
      tiExpr: Expr[TransformerInto[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      fExpr: Expr[From => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    TransformerDefinitionImpl.withFieldComputedImpl('{ ${ tiExpr }.td }, selectorExpr, fExpr) match {
      case '{ $td: TransformerDefinition[From, To, cfg, Flags] } =>
        '{ new TransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
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
      tiExpr: Expr[TransformerInto[From, To, Cfg, Flags]],
      selectorFromExpr: Expr[From => T],
      selectorToExpr: Expr[To => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    TransformerDefinitionImpl.withFieldRenamed('{ ${ tiExpr }.td }, selectorFromExpr, selectorToExpr) match {
      case '{ $td: TransformerDefinition[From, To, cfg, Flags] } =>
        '{ new TransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
    }
  }

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      tiExpr: Expr[TransformerInto[From, To, Cfg, Flags]],
      fExpr: Expr[Inst => To]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    TransformerDefinitionImpl.withCoproductInstance('{ ${ tiExpr }.td }, fExpr) match {
      case '{ $td: TransformerDefinition[From, To, cfg, Flags] } =>
        '{ new TransformerInto[From, To, cfg, Flags](${ tiExpr }.source, ${ td }) }
    }
  }

  def transform[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      ScopeFlags <: TransformerFlags: Type
  ](
      source: Expr[From],
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]]
  )(using Quotes): Expr[To] = {
    TransformerMacros.deriveTotalTransformerResultWithConfig[From, To, Cfg, Flags, ScopeFlags](source, td)
  }

}
