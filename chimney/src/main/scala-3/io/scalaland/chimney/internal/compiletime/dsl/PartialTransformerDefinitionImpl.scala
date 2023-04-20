package io.scalaland.chimney.compiletime.dsl

import io.scalaland.chimney.compiletime.dsl.FieldNameUtils
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}
import io.scalaland.chimney.partial
import io.scalaland.chimney.PartialTransformer

import scala.quoted.*

object PartialTransformerDefinitionImpl {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      valueExpr: Expr[U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
    val fieldName = FieldNameUtils.extractSelectorFieldNameOrAbort(selectorExpr)
    FieldNameUtils.strLiteralType(fieldName).asType match {
      case '[FieldNameUtils.StringBounded[fieldNameT]] =>
        '{ $td.__addOverride($valueExpr).__refineConfig[TransformerCfg.FieldConst[fieldNameT, Cfg]] }
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
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      valueExpr: Expr[partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
    val fieldName = FieldNameUtils.extractSelectorFieldNameOrAbort(selectorExpr)
    FieldNameUtils.strLiteralType(fieldName).asType match {
      case '[FieldNameUtils.StringBounded[fieldNameT]] =>
        '{ $td.__addOverride($valueExpr).__refineConfig[TransformerCfg.FieldConstPartial[fieldNameT, Cfg]] }
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
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      fExpr: Expr[From => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
    val fieldName = FieldNameUtils.extractSelectorFieldNameOrAbort(selectorExpr)
    FieldNameUtils.strLiteralType(fieldName).asType match {
      case '[FieldNameUtils.StringBounded[fieldNameT]] =>
        '{ $td.__addOverride($fExpr).__refineConfig[TransformerCfg.FieldComputed[fieldNameT, Cfg]] }
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
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      selectorExpr: Expr[To => T],
      fExpr: Expr[From => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
    val fieldName = FieldNameUtils.extractSelectorFieldNameOrAbort(selectorExpr)
    FieldNameUtils.strLiteralType(fieldName).asType match {
      case '[FieldNameUtils.StringBounded[fieldNameT]] =>
        '{ $td.__addOverride($fExpr).__refineConfig[TransformerCfg.FieldComputedPartial[fieldNameT, Cfg]] }
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
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      selectorFromExpr: Expr[From => T],
      selectorToExpr: Expr[To => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
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
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      fExpr: Expr[Inst => To]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
    '{ $td.__addInstance($fExpr).__refineConfig[TransformerCfg.CoproductInstance[Inst, To, Cfg]] }
  }

  def withCoproductInstancePartial[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      fExpr: Expr[Inst => partial.Result[To]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] = {
    '{ $td.__addInstance($fExpr).__refineConfig[TransformerCfg.CoproductInstancePartial[Inst, To, Cfg]] }
  }

  def buildTransformer[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      ScopeFlags <: TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]]
  )(using Quotes): Expr[PartialTransformer[From, To]] = {
    '{
      new PartialTransformer[From, To] {
        def transform(src: From, failFast: Boolean): partial.Result[To] =
          partial.Result.fromErrorString("not implemented yet")
      }
    }
  }
}
