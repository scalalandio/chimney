package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime

private[chimney] trait TransformIntoGateway { this: DslDefinitions =>

  import Type.Implicits.*, ChimneyType.Implicits.*
  import runtime.TransformerCfg.*

  final def withFieldConst[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Field: Type
  ](
      ti: Expr[dsls.TransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => Field],
      value: Expr[Field]
  ): Expr[dsls.TransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val fieldName = extractSelectorFieldNameOrFail(selector)
    import fieldName.Underlying as FieldName
    ti.addOverride(value.upcastExpr[Any]).updateCfg[FieldConst[FieldName, Cfg]].forgetCfg
  }

  final def withFieldComputed[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      Field: Type
  ](
      ti: Expr[dsls.TransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => Field],
      f: Expr[From => Field]
  ): Expr[dsls.TransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val fieldName = extractSelectorFieldNameOrFail(selector)
    import fieldName.Underlying as FieldName
    ti.addOverride(f.upcastExpr[Any]).updateCfg[FieldComputed[FieldName, Cfg]].forgetCfg
  }

  final def withFieldRenamed[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      FromField: Type,
      ToField: Type
  ](
      ti: Expr[dsls.TransformerInto[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => FromField],
      selectorTo: Expr[To => ToField]
  ): Expr[dsls.TransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] = {
    val (fieldNameFrom, fieldNameTo) = extractSelectorFieldNamesOrFail(selectorFrom, selectorTo)
    import fieldNameFrom.Underlying as NameFrom, fieldNameTo.Underlying as NameTo
    ti.updateCfg[FieldRelabelled[NameFrom, NameTo, Cfg]].forgetCfg
  }

  final def withCoproductInstance[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      Flags <: runtime.TransformerFlags: Type,
      FromInstance: Type
  ](
      ti: Expr[dsls.TransformerInto[From, To, Cfg, Flags]],
      f: Expr[FromInstance => To]
  ): Expr[dsls.TransformerInto[From, To, ? <: runtime.TransformerCfg, Flags]] =
    ti.addOverride(f.upcastExpr[Any]).updateCfg[CoproductInstance[FromInstance, To, Cfg]].forgetCfg
}
