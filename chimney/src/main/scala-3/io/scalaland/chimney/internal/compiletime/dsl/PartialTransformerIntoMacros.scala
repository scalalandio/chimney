package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.PartialTransformer
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
import io.scalaland.chimney.partial

import scala.quoted.*

object PartialTransformerIntoMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $value)
              .asInstanceOf[PartialTransformerInto[From, To, Const[toPath, Cfg], Flags]]
        }
    }(selector)

  def withFieldConstPartialImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $value)
              .asInstanceOf[PartialTransformerInto[From, To, ConstPartial[toPath, Cfg], Flags]]
        }
    }(selector)

  def withFieldComputedImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $f)
              .asInstanceOf[PartialTransformerInto[From, To, Computed[toPath, Cfg], Flags]]
        }
    }(selector)

  def withFieldComputedPartialImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => partial.Result[U]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $f)
              .asInstanceOf[PartialTransformerInto[From, To, ComputedPartial[toPath, Cfg], Flags]]
        }
    }(selector)

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              $ti.asInstanceOf[
                PartialTransformerInto[From, To, RenamedFrom[fromPath, toPath, Cfg], Flags]
              ]
          }
    }(selectorFrom, selectorTo)

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      f: Expr[Inst => To]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      WithRuntimeDataStore
        .update($ti, $f)
        .asInstanceOf[PartialTransformerInto[From, To, CaseComputed[Path.Match[Inst, Path.Root], Cfg], Flags]]
    }

  def withCoproductInstancePartialImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      f: Expr[Inst => partial.Result[To]]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      WithRuntimeDataStore
        .update($ti, $f)
        .asInstanceOf[PartialTransformerInto[From, To, CaseComputedPartial[Path.Match[Inst, Path.Root], Cfg], Flags]]
    }

  def withConstructorImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $f)
              .asInstanceOf[PartialTransformerInto[From, To, Constructor[args, Path.Root, Cfg], Flags]]
        }
    }(f)

  def withConstructorPartialImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[PartialTransformerInto[From, To, Cfg, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $f)
              .asInstanceOf[PartialTransformerInto[From, To, ConstructorPartial[args, Path.Root, Cfg], Flags]]
        }
    }(f)

  def transform[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      ImplicitScopeFlags <: TransformerFlags: Type
  ](
      source: Expr[From],
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]],
      failFast: Boolean
  )(using Quotes): Expr[partial.Result[To]] =
    TransformerMacros.derivePartialTransformerResultWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](
      source,
      td,
      failFast
    )
}
