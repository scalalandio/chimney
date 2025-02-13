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
              .asInstanceOf[TransformerDefinition[From, To, Computed[Path.Root, toPath, Overrides], Flags]]
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
                .asInstanceOf[TransformerDefinition[From, To, Computed[fromPath, toPath, Overrides], Flags]]
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
                Renamed[fromPath, toPath, Overrides],
                Flags
              ]]
          }
    }(selectorFrom, selectorTo)

  def withFieldUnusedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fromPath <: Path] =>
        (_: Type[fromPath]) ?=>
          '{
            ${ ti }.asInstanceOf[TransformerDefinition[From, To, Unused[fromPath, Overrides], Flags]]
        }
    }(selectorFrom)

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
          Computed[Path.SourceMatching[Path.Root, Subtype], Path.Root, Overrides],
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
          Renamed[Path.SourceMatching[Path.Root, FromSubtype], Path.Matching[Path.Root, ToSubtype], Overrides],
          Flags
        ]]
    }

  def withSealedSubtypeUnmatchedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            ${ ti }.asInstanceOf[TransformerDefinition[From, To, Unmatched[toPath, Overrides], Flags]]
        }
    }(selectorTo)

  def withFallbackImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromFallback: Type
  ](
      ti: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      WithRuntimeDataStore
        .update($ti, $fallback)
        .asInstanceOf[TransformerDefinition[From, To, Fallback[FromFallback, Path.Root, Overrides], Flags]]
    }

  def withFallbackFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      FromFallback: Type
  ](
      ti: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fromPath <: Path] =>
        (_: Type[fromPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $fallback)
              .asInstanceOf[TransformerDefinition[From, To, Fallback[FromFallback, fromPath, Overrides], Flags]]
        }
    }(selectorFrom)

  def withConstructorImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $f)
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
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
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
                    .update($td, $f)
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
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[TransformerSourceFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, ? <: Path]] =
    DslMacroUtils()
      .applyFieldNameType {
        [fromPath <: Path] =>
          (_: Type[fromPath]) ?=>
            '{ TransformerSourceFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, fromPath]($td) }
      }(selectorFrom)

  def withTargetFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[TransformerTargetFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, ? <: Path]] =
    DslMacroUtils()
      .applyFieldNameType {
        [toPath <: Path] =>
          (_: Type[toPath]) ?=>
            '{ TransformerTargetFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, toPath]($td) }
      }(selectorTo)
}
