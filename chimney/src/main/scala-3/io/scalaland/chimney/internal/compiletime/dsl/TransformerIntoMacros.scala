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
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[TransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $value)
              .asInstanceOf[TransformerInto[From, To, Const[toPath, Cfg], Flags]]
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
      ti: Expr[TransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [toPath <: Path] =>
        (_: Type[toPath]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $f)
              .asInstanceOf[TransformerInto[From, To, Computed[toPath, Cfg], Flags]]
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
      ti: Expr[TransformerInto[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              $ti.asInstanceOf[TransformerInto[From, To, RenamedFrom[fromPath, toPath, Cfg], Flags]]
          }
    }(selectorFrom, selectorTo)

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      ti: Expr[TransformerInto[From, To, Cfg, Flags]],
      f: Expr[Inst => To]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    '{
      WithRuntimeDataStore
        .update($ti, $f)
        .asInstanceOf[TransformerInto[From, To, CaseComputed[Path.Match[Inst, Path.Root], Cfg], Flags]]
    }

  def withConstructorImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[TransformerInto[From, To, Cfg, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyConstructorType {
      [args <: ArgumentLists] =>
        (_: Type[args]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $f)
              .asInstanceOf[TransformerInto[From, To, Constructor[args, Path.Root, Cfg], Flags]]
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
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]]
  )(using Quotes): Expr[To] =
    TransformerMacros.deriveTotalTransformerResultWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](source, td)
}
