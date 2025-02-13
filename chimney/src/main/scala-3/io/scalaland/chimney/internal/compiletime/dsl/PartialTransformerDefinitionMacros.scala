package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.utils.DslMacroUtils
import io.scalaland.chimney.internal.runtime.{
  ArgumentLists,
  FunctionEitherToResult,
  Path,
  TransformerFlags,
  TransformerOverrides,
  WithRuntimeDataStore
}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*
import io.scalaland.chimney.partial

import scala.quoted.*

object PartialTransformerDefinitionMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $value)
              .asInstanceOf[PartialTransformerDefinition[From, To, Const[toPath, Overrides], Flags]]
        }
    }(selector)

  def withFieldConstPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      value: Expr[partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $value)
              .asInstanceOf[PartialTransformerDefinition[
                From,
                To,
                ConstPartial[toPath, Overrides],
                Flags
              ]]
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
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $f)
              .asInstanceOf[PartialTransformerDefinition[From, To, Computed[Path.Root, toPath, Overrides], Flags]]
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
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[S => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              WithRuntimeDataStore
                .update($td, $f)
                .asInstanceOf[PartialTransformerDefinition[From, To, Computed[fromPath, toPath, Overrides], Flags]]
          }
    }(selectorFrom, selectorTo)

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[From => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $f)
              .asInstanceOf[PartialTransformerDefinition[
                From,
                To,
                ComputedPartial[Path.Root, toPath, Overrides],
                Flags
              ]]
        }
    }(selector)

  def withFieldComputedPartialFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      S: Type,
      T: Type,
      U: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => S],
      selectorTo: Expr[To => T],
      f: Expr[S => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              WithRuntimeDataStore
                .update($td, $f)
                .asInstanceOf[PartialTransformerDefinition[
                  From,
                  To,
                  ComputedPartial[fromPath, toPath, Overrides],
                  Flags
                ]]
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
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              $td.asInstanceOf[
                PartialTransformerDefinition[From, To, Renamed[fromPath, toPath, Overrides], Flags]
              ]
          }
    }(selectorFrom, selectorTo)

  def withFieldUnusedImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      ti: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fromPath <: Path] =>
        (_: Type[fromPath]) ?=>
          '{
            ${ ti }.asInstanceOf[PartialTransformerDefinition[From, To, Unused[fromPath, Overrides], Flags]]
        }
    }(selectorFrom)

  def withSealedSubtypeHandledImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Subtype => To]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      WithRuntimeDataStore
        .update($td, $f)
        .asInstanceOf[PartialTransformerDefinition[
          From,
          To,
          Computed[Path.SourceMatching[Path.Root, Subtype], Path.Root, Overrides],
          Flags
        ]]
    }

  def withSealedSubtypeHandledPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Subtype: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Subtype => partial.Result[To]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      WithRuntimeDataStore
        .update($td, $f)
        .asInstanceOf[PartialTransformerDefinition[
          From,
          To,
          ComputedPartial[Path.SourceMatching[Path.Root, Subtype], Path.Root, Overrides],
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
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      $td
        .asInstanceOf[PartialTransformerDefinition[
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
      ti: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            ${ ti }.asInstanceOf[PartialTransformerDefinition[From, To, Unmatched[toPath, Overrides], Flags]]
        }
    }(selectorTo)

  def withFallbackImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FromFallback: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      WithRuntimeDataStore
        .update($td, $fallback)
        .asInstanceOf[PartialTransformerDefinition[From, To, Fallback[FromFallback, Path.Root, Overrides], Flags]]
    }

  def withFallbackFromImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      FromFallback: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T],
      fallback: Expr[FromFallback]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fromPath <: Path] =>
        (_: Type[fromPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $fallback)
              .asInstanceOf[PartialTransformerDefinition[From, To, Fallback[FromFallback, fromPath, Overrides], Flags]]
        }
    }(selectorFrom)

  def withConstructorImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $f)
              .asInstanceOf[PartialTransformerDefinition[From, To, Constructor[args, Path.Root, Overrides], Flags]]
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
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          DslMacroUtils().applyFieldNameType {
            [toPath <: Path] =>
              (_: Type[toPath]) ?=>
                '{
                  WithRuntimeDataStore
                    .update($td, $f)
                    .asInstanceOf[PartialTransformerDefinition[
                      From,
                      To,
                      Constructor[args, toPath, Overrides],
                      Flags
                    ]]
              }
          }(selector)
    }(f)

  def withConstructorPartialImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $f)
              .asInstanceOf[PartialTransformerDefinition[
                From,
                To,
                ConstructorPartial[args, Path.Root, Overrides],
                Flags
              ]]
        }
    }(f)

  def withConstructorPartialToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          DslMacroUtils().applyFieldNameType {
            [toPath <: Path] =>
              (_: Type[toPath]) ?=>
                '{
                  WithRuntimeDataStore
                    .update($td, $f)
                    .asInstanceOf[PartialTransformerDefinition[
                      From,
                      To,
                      ConstructorPartial[args, toPath, Overrides],
                      Flags
                    ]]
              }
          }(selector)
    }(f)

  def withConstructorEitherImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          '{
            WithRuntimeDataStore
              .update(
                $td,
                FunctionEitherToResult.lift[Ctor, Any]($f)(
                  ${ Expr.summon[FunctionEitherToResult[Ctor]].get }.asInstanceOf[FunctionEitherToResult.Aux[Ctor, Any]]
                )
              )
              .asInstanceOf[PartialTransformerDefinition[
                From,
                To,
                ConstructorPartial[args, Path.Root, Overrides],
                Flags
              ]]
        }
    }(f)

  def withConstructorEitherToImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      Ctor: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selector: Expr[To => T],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          DslMacroUtils().applyFieldNameType {
            [toPath <: Path] =>
              (_: Type[toPath]) ?=>
                '{
                  WithRuntimeDataStore
                    .update(
                      $td,
                      FunctionEitherToResult.lift[Ctor, Any]($f)(
                        ${ Expr.summon[FunctionEitherToResult[Ctor]].get }
                          .asInstanceOf[FunctionEitherToResult.Aux[Ctor, Any]]
                      )
                    )
                    .asInstanceOf[PartialTransformerDefinition[
                      From,
                      To,
                      ConstructorPartial[args, toPath, Overrides],
                      Flags
                    ]]
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
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[From => T]
  )(using
      Quotes
  ): Expr[TransformerSourceFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, ? <: Path]] =
    DslMacroUtils()
      .applyFieldNameType {
        [fromPath <: Path] =>
          (_: Type[fromPath]) ?=>
            '{ TransformerSourceFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, fromPath]($td) }
      }(selectorFrom)

  def withTargetFlagImpl[
      From: Type,
      To: Type,
      Overrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[To => T]
  )(using
      Quotes
  ): Expr[TransformerTargetFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, ? <: Path]] =
    DslMacroUtils()
      .applyFieldNameType {
        [toPath <: Path] =>
          (_: Type[toPath]) ?=>
            '{ TransformerTargetFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, toPath]($td) }
      }(selectorTo)
}
