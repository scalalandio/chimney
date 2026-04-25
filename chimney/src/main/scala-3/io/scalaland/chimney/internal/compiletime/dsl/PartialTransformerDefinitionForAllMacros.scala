package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.dsl.utils.DslMacroUtils
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides, WithRuntimeDataStore}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*
import io.scalaland.chimney.partial

import scala.quoted.*

object PartialTransformerDefinitionForAllMacros {

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromMatch: Type,
      ToMatch: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selectorFrom: Expr[FromMatch => T],
      selectorTo: Expr[ToMatch => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] => (_: Type[fromPath]) ?=> (_: Type[toPath]) ?=>
        '{
          new PartialTransformerDefinition[
            From,
            To,
            ForAll[FromMatch, ToMatch, RenamedFrom[fromPath, toPath, Empty], Overrides],
            Flags
          ]($td.runtimeData)
        }
    }(selectorFrom, selectorTo)

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromMatch: Type,
      ToMatch: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      value: Expr[U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType { [toPath <: Path] => (_: Type[toPath]) ?=>
      '{
        new PartialTransformerDefinition[
          From,
          To,
          ForAll[FromMatch, ToMatch, Const[toPath, Empty], Overrides],
          Flags
        ]($value +: $td.runtimeData)
      }
    }(selector)

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromMatch: Type,
      ToMatch: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      f: Expr[FromMatch => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType { [toPath <: Path] => (_: Type[toPath]) ?=>
      '{
        new PartialTransformerDefinition[
          From,
          To,
          ForAll[FromMatch, ToMatch, Computed[toPath, Empty], Overrides],
          Flags
        ]($f +: $td.runtimeData)
      }
    }(selector)

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromMatch: Type,
      ToMatch: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      f: Expr[FromMatch => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType { [toPath <: Path] => (_: Type[toPath]) ?=>
      '{
        new PartialTransformerDefinition[
          From,
          To,
          ForAll[FromMatch, ToMatch, ComputedPartial[toPath, Empty], Overrides],
          Flags
        ]($f +: $td.runtimeData)
      }
    }(selector)
}
