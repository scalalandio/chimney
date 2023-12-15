package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.utils.DslMacroUtils
import io.scalaland.chimney.internal.runtime.{
  ArgumentLists,
  Path,
  TransformerCfg,
  TransformerFlags,
  WithRuntimeDataStore
}
import io.scalaland.chimney.internal.runtime.TransformerCfg.*

import scala.quoted.*

object TransformerIntoMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[TransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerCfg, Flags]] = DslMacroUtils().applyFieldNameType {
    [fieldNameT <: Path] =>
      (_: Type[fieldNameT]) ?=>
        '{
          WithRuntimeDataStore
            .update($ti, $value)
            .asInstanceOf[TransformerInto[From, To, FieldConst[fieldNameT, Cfg], Flags]]
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
      ti: Expr[TransformerInto[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerCfg, Flags]] = DslMacroUtils().applyFieldNameType {
    [fieldNameT <: Path] =>
      (_: Type[fieldNameT]) ?=>
        '{
          WithRuntimeDataStore
            .update($ti, $f)
            .asInstanceOf[TransformerInto[From, To, FieldComputed[fieldNameT, Cfg], Flags]]
      }
  }(selector)

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      ti: Expr[TransformerInto[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerCfg, Flags]] = DslMacroUtils().applyFieldNameTypes {
    [fieldNameFromT <: Path, fieldNameToT <: Path] =>
      (_: Type[fieldNameFromT]) ?=>
        (_: Type[fieldNameToT]) ?=>
          '{
            $ti.asInstanceOf[TransformerInto[From, To, FieldRelabelled[fieldNameFromT, fieldNameToT, Cfg], Flags]]
        }
  }(selectorFrom, selectorTo)

  def withCoproductInstanceImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      ti: Expr[TransformerInto[From, To, Cfg, Flags]],
      f: Expr[Inst => To]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerCfg, Flags]] =
    '{
      WithRuntimeDataStore
        .update($ti, $f)
        .asInstanceOf[TransformerInto[From, To, CoproductInstance[Inst, To, Cfg], Flags]]
    }

  def withConstructorImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[TransformerInto[From, To, Cfg, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerInto[From, To, ? <: TransformerCfg, Flags]] = DslMacroUtils().applyConstructorType {
    [ctor <: ArgumentLists] =>
      (_: Type[ctor]) ?=>
        '{
          WithRuntimeDataStore
            .update($ti, $f)
            .asInstanceOf[TransformerInto[From, To, Constructor[ctor, To, Cfg], Flags]]
      }
  }(f)

  def transform[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      ImplicitScopeFlags <: TransformerFlags: Type
  ](
      source: Expr[From],
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]]
  )(using Quotes): Expr[To] =
    TransformerMacros.deriveTotalTransformerResultWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](source, td)
}
