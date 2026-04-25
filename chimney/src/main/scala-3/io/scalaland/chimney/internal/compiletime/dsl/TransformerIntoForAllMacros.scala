package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.dsl.utils.DslMacroUtils
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides, WithRuntimeDataStore}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.quoted.*

object TransformerIntoForAllMacros {

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
      ti: Expr[TransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selectorFrom: Expr[FromMatch => T],
      selectorTo: Expr[ToMatch => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] => (_: Type[fromPath]) ?=> (_: Type[toPath]) ?=>
        '{
          new TransformerInto[From, To, ForAll[FromMatch, ToMatch, RenamedFrom[fromPath, toPath, Empty], Overrides], Flags](
            $ti.source,
            $ti.td.asInstanceOf[TransformerDefinition[From, To, ForAll[FromMatch, ToMatch, RenamedFrom[fromPath, toPath, Empty], Overrides], Flags]]
          )
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
      ti: Expr[TransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType { [toPath <: Path] => (_: Type[toPath]) ?=>
      '{
        val updatedTd = WithRuntimeDataStore
          .update($ti.td, $value)
          .asInstanceOf[TransformerDefinition[From, To, ForAll[FromMatch, ToMatch, Const[toPath, Empty], Overrides], Flags]]
        new TransformerInto[From, To, ForAll[FromMatch, ToMatch, Const[toPath, Empty], Overrides], Flags](
          $ti.source,
          updatedTd
        )
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
      ti: Expr[TransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
      selector: Expr[ToMatch => T],
      f: Expr[FromMatch => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType { [toPath <: Path] => (_: Type[toPath]) ?=>
      '{
        val updatedTd = WithRuntimeDataStore
          .update($ti.td, $f)
          .asInstanceOf[TransformerDefinition[From, To, ForAll[FromMatch, ToMatch, Computed[toPath, Empty], Overrides], Flags]]
        new TransformerInto[From, To, ForAll[FromMatch, ToMatch, Computed[toPath, Empty], Overrides], Flags](
          $ti.source,
          updatedTd
        )
      }
    }(selector)
}
