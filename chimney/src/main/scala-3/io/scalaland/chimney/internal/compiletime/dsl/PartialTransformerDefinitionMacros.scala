package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.utils.DslMacroUtils
import io.scalaland.chimney.internal.runtime.{Path, TransformerCfg, TransformerFlags, WithRuntimeDataStore}
import io.scalaland.chimney.internal.runtime.TransformerCfg.*
import io.scalaland.chimney.partial

import scala.quoted.*

object PartialTransformerDefinitionMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fieldNameT <: Path] =>
        (_: Type[fieldNameT]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $value)
              .asInstanceOf[PartialTransformerDefinition[From, To, FieldConst[fieldNameT, Cfg], Flags]]
        }
    }(selector)

  def withFieldConstPartialImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fieldNameT <: Path] =>
        (_: Type[fieldNameT]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $value)
              .asInstanceOf[PartialTransformerDefinition[
                From,
                To,
                FieldConstPartial[fieldNameT, Cfg],
                Flags
              ]]
        }
    }(selector)

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fieldNameT <: Path] =>
        (_: Type[fieldNameT]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $f)
              .asInstanceOf[PartialTransformerDefinition[From, To, FieldComputed[fieldNameT, Cfg], Flags]]
        }
    }(selector)

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fieldNameT <: Path] =>
        (_: Type[fieldNameT]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $f)
              .asInstanceOf[PartialTransformerDefinition[
                From,
                To,
                FieldComputedPartial[fieldNameT, Cfg],
                Flags
              ]]
        }
    }(selector)

  def withFieldRenamed[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fieldNameFromT <: Path, fieldNameToT <: Path] =>
        (_: Type[fieldNameFromT]) ?=>
          (_: Type[fieldNameToT]) ?=>
            '{
              $td.asInstanceOf[
                PartialTransformerDefinition[From, To, FieldRelabelled[fieldNameFromT, fieldNameToT, Cfg], Flags]
              ]
          }
    }(selectorFrom, selectorTo)

  def withCoproductInstance[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      f: Expr[Inst => To]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    '{
      WithRuntimeDataStore
        .update($td, $f)
        .asInstanceOf[PartialTransformerDefinition[From, To, CoproductInstance[Inst, To, Cfg], Flags]]
    }

  def withCoproductInstancePartial[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      f: Expr[Inst => partial.Result[To]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    '{
      WithRuntimeDataStore
        .update($td, $f)
        .asInstanceOf[PartialTransformerDefinition[From, To, CoproductInstancePartial[Inst, To, Cfg], Flags]]
    }

  def buildTransformer[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      ImplicitScopeFlags <: TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]]
  )(using Quotes): Expr[PartialTransformer[From, To]] =
    TransformerMacros.derivePartialTransformerWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](td)
}
