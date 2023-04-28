package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.FieldNameUtils
import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}

import scala.quoted.*

object TransformerDefinitionImpl {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      valueExpr: Expr[U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
    val fieldName = FieldNameUtils.extractSelectorFieldNameOrAbort(selectorExpr)
    FieldNameUtils.strLiteralType(fieldName).asType match {
      case '[FieldNameUtils.StringBounded[fieldNameT]] =>
        '{ $td.__addOverride($valueExpr).__refineConfig[TransformerCfg.FieldConst[fieldNameT, Cfg]] }
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
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      fExpr: Expr[From => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
    val fieldName = FieldNameUtils.extractSelectorFieldNameOrAbort(selectorExpr)
    FieldNameUtils.strLiteralType(fieldName).asType match {
      case '[FieldNameUtils.StringBounded[fieldNameT]] =>
        '{ $td.__addOverride($fExpr).__refineConfig[TransformerCfg.FieldComputed[fieldNameT, Cfg]] }
    }
  }

  def withFieldRenamed[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]],
      selectorFromExpr: Expr[From => T],
      selectorToExpr: Expr[To => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
    val (fieldNameFrom, fieldNameTo) = FieldNameUtils.extractSelectorFieldNamesOrAbort(selectorFromExpr, selectorToExpr)
    (FieldNameUtils.strLiteralType(fieldNameFrom).asType, FieldNameUtils.strLiteralType(fieldNameTo).asType) match {
      case ('[FieldNameUtils.StringBounded[fieldNameFromT]], '[FieldNameUtils.StringBounded[fieldNameToT]]) =>
        '{ $td.__refineConfig[TransformerCfg.FieldRelabelled[fieldNameFromT, fieldNameToT, Cfg]] }
    }
  }

  def withCoproductInstance[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]],
      fExpr: Expr[Inst => To]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
    '{ $td.__addInstance($fExpr).__refineConfig[TransformerCfg.CoproductInstance[Inst, To, Cfg]] }
  }

  def buildTransformer[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      ScopeFlags <: TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]]
  )(using Quotes): Expr[Transformer[From, To]] =
    TransformerMacros.deriveTotalTransformerWithConfig[From, To, Cfg, Flags, ScopeFlags](td)
}
