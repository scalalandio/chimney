package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
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

object TransformerIntoMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $value)
              .asInstanceOf[TransformerInto[From, To, Const[toPath, Overrides], Flags]]
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
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $f)
              .asInstanceOf[TransformerInto[From, To, Computed[Path.Root, toPath, Overrides], Flags]]
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
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[S => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              WithRuntimeDataStore
                .update($ti, $f)
                .asInstanceOf[TransformerInto[From, To, Computed[fromPath, toPath, Overrides], Flags]]
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
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              $ti.asInstanceOf[TransformerInto[From, To, Renamed[fromPath, toPath, Overrides], Flags]]
          }
    }(selectorFrom, selectorTo)

  def withFieldUnusedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fromPath <: Path] =>
        (_: Type[fromPath]) ?=>
          '{
            $ti.asInstanceOf[TransformerInto[From, To, Unused[fromPath, Overrides], Flags]]
        }
    }(selectorFrom)

  def withSealedSubtypeHandledImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      f: Expr[Subtype => To]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      WithRuntimeDataStore
        .update($ti, $f)
        .asInstanceOf[TransformerInto[
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
      td: Expr[TransformerInto[From, To, Overrides, Flags]]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      $td
        .asInstanceOf[TransformerInto[
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
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            ${ ti }.asInstanceOf[TransformerInto[From, To, Unmatched[toPath, Overrides], Flags]]
        }
    }(selectorTo)

  def withConstructorImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $f)
              .asInstanceOf[TransformerInto[From, To, Constructor[args, Path.Root, Overrides], Flags]]
        }
    }(f)

  def withFallbackImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromFallback: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      WithRuntimeDataStore
        .update($ti, $fallback)
        .asInstanceOf[TransformerInto[From, To, Fallback[FromFallback, Path.Root, Overrides], Flags]]
    }

  def withFallbackFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      FromFallback: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fromPath <: Path] =>
        (_: Type[fromPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $fallback)
              .asInstanceOf[TransformerInto[From, To, Fallback[FromFallback, fromPath, Overrides], Flags]]
        }
    }(selectorFrom)

  def withConstructorToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          DslMacroUtils().applyFieldNameType {
            [toPath <: Path] =>
              (_: Type[toPath]) ?=>
                '{
                  WithRuntimeDataStore
                    .update($ti, $f)
                    .asInstanceOf[TransformerInto[From, To, Constructor[args, toPath, Overrides], Flags]]
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
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[TransformerSourceFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, ? <: Path]] =
    DslMacroUtils()
      .applyFieldNameType {
        [fromPath <: Path] =>
          (_: Type[fromPath]) ?=>
            '{ TransformerSourceFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, fromPath]($ti) }
      }(selectorFrom)

  def withTargetFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[TransformerInto[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[TransformerTargetFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, ? <: Path]] =
    DslMacroUtils()
      .applyFieldNameType {
        [toPath <: Path] =>
          (_: Type[toPath]) ?=>
            '{ TransformerTargetFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, toPath]($ti) }
      }(selectorTo)
}
