package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime

private[chimney] trait TransformerDefinitionGateway { this: DslDefinitions =>

  import Type.Implicits.*, ChimneyType.Implicits.*
  import runtime.TransformerCfg.*

  final def withFieldConst[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Field: Type
  ](
      td: Expr[dsls.TransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => Field],
      value: Expr[Field]
  ): Expr[dsls.TransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val fieldName = extractSelectorFieldNameOrFail(selector)
    import fieldName.Underlying as FieldName
    td.addOverride(value.upcastExpr[Any]).updateCfg[FieldConst[FieldName, Cfg]].forgetCfg
  }

  final def withFieldComputed[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Field: Type
  ](
      td: Expr[dsls.TransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => Field],
      f: Expr[From => Field]
  ): Expr[dsls.TransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val fieldName = extractSelectorFieldNameOrFail(selector)
    import fieldName.Underlying as FieldName
    td.addOverride(f.upcastExpr[Any]).updateCfg[FieldComputed[FieldName, Cfg]].forgetCfg
  }

  final def withFieldRenamed[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      FromField: Type,
      ToField: Type
  ](
      td: Expr[dsls.TransformerDefinition[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => FromField],
      selectorTo: Expr[To => ToField]
  ): Expr[dsls.TransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val (fieldNameFrom, fieldNameTo) = extractSelectorFieldNamesOrFail(selectorFrom, selectorTo)
    import fieldNameFrom.Underlying as NameFrom, fieldNameTo.Underlying as NameTo
    td.updateCfg[FieldRelabelled[NameFrom, NameTo, Cfg]].forgetCfg
  }

  final def withCoproductInstance[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      FromInstance: Type
  ](
      td: Expr[dsls.TransformerDefinition[From, To, Cfg, Flags]],
      f: Expr[FromInstance => To]
  ): Expr[dsls.TransformerDefinition[From, To, ? <: runtime.TransformerCfg, Flags]] =
    td.addOverride(f.upcastExpr[Any]).updateCfg[CoproductInstance[FromInstance, To, Cfg]].forgetCfg
}
