package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.dsl.utils.DslMacroUtils
import io.scalaland.chimney.internal.runtime.{
  ArgumentLists,
  Path,
  TransformerFlags,
  TransformerOverrides,
  WithRuntimeDataStore
}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.quoted.*

object TransformerDefinitionMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $value)
              .asInstanceOf[TransformerDefinition[From, To, Const[toPath, Overrides], Flags]]
        }
    }(selector)

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $f)
              .asInstanceOf[TransformerDefinition[From, To, Computed[toPath, Overrides], Flags]]
        }
    }(selector)

  def withFieldComputedFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      S: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[S => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              WithRuntimeDataStore
                .update($td, $f)
                .asInstanceOf[TransformerDefinition[From, To, ComputedFrom[fromPath, toPath, Overrides], Flags]]
          }
    }(selectorFrom, selectorTo)

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              $td.asInstanceOf[TransformerDefinition[
                From,
                To,
                RenamedFrom[fromPath, toPath, Overrides],
                Flags
              ]]
          }
    }(selectorFrom, selectorTo)

  def withSealedSubtypeHandledImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Subtype => To]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      WithRuntimeDataStore
        .update($td, $f)
        .asInstanceOf[TransformerDefinition[
          From,
          To,
          CaseComputed[Path.SourceMatching[Path.Root, Subtype], Overrides],
          Flags
        ]]
    }

  def withSealedSubtypeRenamedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromSubtype: Type,
      ToSubtype: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      $td
        .asInstanceOf[TransformerDefinition[
          From,
          To,
          RenamedTo[Path.SourceMatching[Path.Root, FromSubtype], Path.Matching[Path.Root, ToSubtype], Overrides],
          Flags
        ]]
    }

  def withConstructorImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $f)
              .asInstanceOf[TransformerDefinition[From, To, Constructor[args, Path.Root, Overrides], Flags]]
        }
    }(f)

  def withConstructorToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      ti: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          DslMacroUtils().applyFieldNameType {
            [toPath <: Path] =>
              (_: Type[toPath]) ?=>
                '{
                  WithRuntimeDataStore
                    .update($ti, $f)
                    .asInstanceOf[TransformerDefinition[From, To, Constructor[args, toPath, Overrides], Flags]]
              }
          }(selector)
    }(f)

  def withSourceFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[TransformerSourceFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, ? <: Path]] =
    DslMacroUtils()
      .applyFieldNameType {
        [fromPath <: Path] =>
          (_: Type[fromPath]) ?=>
            '{ TransformerSourceFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, fromPath]($ti) }
      }(selectorFrom)

  def withTargetFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[TransformerTargetFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, ? <: Path]] =
    DslMacroUtils()
      .applyFieldNameType {
        [toPath <: Path] =>
          (_: Type[toPath]) ?=>
            '{ TransformerTargetFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, toPath]($ti) }
      }(selectorTo)
}
