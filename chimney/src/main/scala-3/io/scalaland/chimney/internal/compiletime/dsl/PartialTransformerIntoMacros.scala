package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags, WithRuntimeDataStore}
import io.scalaland.chimney.internal.runtime.TransformerCfg.*
import io.scalaland.chimney.partial

import scala.quoted.*

object PartialTransformerIntoMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    val fieldName = FieldNameUtils.extractSelectorFieldNameOrAbort(selector)
    FieldNameUtils.strLiteralType(fieldName).asType match {
      case '[FieldNameUtils.StringBounded[fieldNameT]] =>
        '{
          WithRuntimeDataStore
            .update($ti, $value)
            .asInstanceOf[PartialTransformerInto[From, To, FieldConst[fieldNameT, Cfg], Flags]]
        }
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
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    val fieldName = FieldNameUtils.extractSelectorFieldNameOrAbort(selector)
    FieldNameUtils.strLiteralType(fieldName).asType match {
      case '[FieldNameUtils.StringBounded[fieldNameT]] =>
        '{
          WithRuntimeDataStore
            .update($ti, $value)
            .asInstanceOf[PartialTransformerInto[From, To, FieldConstPartial[fieldNameT, Cfg], Flags]]
        }
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
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    val fieldName = FieldNameUtils.extractSelectorFieldNameOrAbort(selector)
    FieldNameUtils.strLiteralType(fieldName).asType match {
      case '[FieldNameUtils.StringBounded[fieldNameT]] =>
        '{
          WithRuntimeDataStore
            .update($ti, $f)
            .asInstanceOf[PartialTransformerInto[From, To, FieldComputed[fieldNameT, Cfg], Flags]]
        }
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
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    val fieldName = FieldNameUtils.extractSelectorFieldNameOrAbort(selector)
    FieldNameUtils.strLiteralType(fieldName).asType match {
      case '[FieldNameUtils.StringBounded[fieldNameT]] =>
        '{
          WithRuntimeDataStore
            .update($ti, $f)
            .asInstanceOf[PartialTransformerInto[From, To, FieldComputedPartial[fieldNameT, Cfg], Flags]]
        }
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
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] = {
    val (fieldNameFrom, fieldNameTo) = FieldNameUtils.extractSelectorFieldNamesOrAbort(selectorFrom, selectorTo)
    (FieldNameUtils.strLiteralType(fieldNameFrom).asType, FieldNameUtils.strLiteralType(fieldNameTo).asType) match {
      case ('[FieldNameUtils.StringBounded[fieldNameFromT]], '[FieldNameUtils.StringBounded[fieldNameToT]]) =>
        '{
          $ti
            .asInstanceOf[PartialTransformerInto[From, To, FieldRelabelled[fieldNameFromT, fieldNameToT, Cfg], Flags]]
        }
    }
  }

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      f: Expr[Inst => To]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] =
    '{
      WithRuntimeDataStore
        .update($ti, $f)
        .asInstanceOf[PartialTransformerInto[From, To, CoproductInstance[Inst, To, Cfg], Flags]]
    }

  def withCoproductInstancePartialImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      f: Expr[Inst => partial.Result[To]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerCfg, Flags]] =
    '{
      WithRuntimeDataStore
        .update($ti, $f)
        .asInstanceOf[PartialTransformerInto[From, To, CoproductInstancePartial[Inst, To, Cfg], Flags]]
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
