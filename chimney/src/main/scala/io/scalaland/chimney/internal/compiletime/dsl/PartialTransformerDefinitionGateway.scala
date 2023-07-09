package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

private[chimney] trait PartialTransformerDefinitionGateway { this: DslDefinitions =>

  import Type.Implicits.*, ChimneyType.Implicits.*
  import runtime.TransformerCfg.*

  final def withFieldConst[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Field: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => Field],
      value: Expr[Field]
  ): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val fieldName = extractSelectorFieldNameOrFail(selector)
    import fieldName.Underlying as FieldName
    ptd.addOverride(value.upcastExpr[Any]).updateCfg[FieldConst[FieldName, Cfg]].forgetCfg
  }

  final def withFieldConstPartial[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Field: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => Field],
      value: Expr[partial.Result[Field]]
  ): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val fieldName = extractSelectorFieldNameOrFail(selector)
    import fieldName.Underlying as FieldName
    ptd.addOverride(value.upcastExpr[Any]).updateCfg[FieldConstPartial[FieldName, Cfg]].forgetCfg
  }

  final def withFieldComputed[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Field: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => Field],
      f: Expr[From => Field]
  ): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val fieldName = extractSelectorFieldNameOrFail(selector)
    import fieldName.Underlying as FieldName
    ptd.addOverride(f.upcastExpr[Any]).updateCfg[FieldComputed[FieldName, Cfg]].forgetCfg
  }

  final def withFieldComputedPartial[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Field: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => Field],
      f: Expr[From => partial.Result[Field]]
  ): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val fieldName = extractSelectorFieldNameOrFail(selector)
    import fieldName.Underlying as FieldName
    ptd.addOverride(f.upcastExpr[Any]).updateCfg[FieldComputedPartial[FieldName, Cfg]].forgetCfg
  }

  final def withFieldRenamed[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      FromField: Type,
      ToField: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => FromField],
      selectorTo: Expr[To => ToField]
  ): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val (fieldNameFrom, fieldNameTo) = extractSelectorFieldNamesOrFail(selectorFrom, selectorTo)
    import fieldNameFrom.Underlying as NameFrom, fieldNameTo.Underlying as NameTo
    ptd.updateCfg[FieldRelabelled[NameFrom, NameTo, Cfg]].forgetCfg
  }

  final def withCoproductInstance[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      FromInstance: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      f: Expr[FromInstance => To]
  ): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    ptd.addOverride(f.upcastExpr[Any]).updateCfg[CoproductInstance[FromInstance, To, Cfg]].forgetCfg

  final def withCoproductInstancePartial[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      FromInstance: Type
  ](
      ptd: Expr[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]],
      f: Expr[FromInstance => partial.Result[To]]
  ): Expr[dsls.PartialTransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    ptd.addOverride(f.upcastExpr[Any]).updateCfg[CoproductInstancePartial[FromInstance, To, Cfg]].forgetCfg
}
