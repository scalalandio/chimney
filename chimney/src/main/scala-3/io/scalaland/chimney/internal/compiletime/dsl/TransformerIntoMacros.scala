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
              .asInstanceOf[TransformerInto[From, To, Computed[toPath, Overrides], Flags]]
        }
    }(selector)

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
              $ti.asInstanceOf[TransformerInto[From, To, RenamedFrom[fromPath, toPath, Overrides], Flags]]
          }
    }(selectorFrom, selectorTo)

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
      td: Expr[TransformerInto[From, To, Overrides, Flags]]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      $td
        .asInstanceOf[TransformerInto[
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
}
