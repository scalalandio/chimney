package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.Transformer
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

object TransformerDefinitionMacros {

  def withFieldConstImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      value: Expr[U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fieldNameT <: Path] =>
        (_: Type[fieldNameT]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $value)
              .asInstanceOf[TransformerDefinition[From, To, FieldConst[fieldNameT, Cfg], Flags]]
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
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]],
      selector: Expr[To => T],
      f: Expr[From => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    DslMacroUtils().applyFieldNameType {
      [fieldNameT <: Path] =>
        (_: Type[fieldNameT]) ?=>
          '{
            WithRuntimeDataStore
              .update($td, $f)
              .asInstanceOf[TransformerDefinition[From, To, FieldComputed[fieldNameT, Cfg], Flags]]
        }
    }(selector)

  def withFieldRenamed[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fieldNameFromT <: Path, fieldNameToT <: Path] =>
        (_: Type[fieldNameFromT]) ?=>
          (_: Type[fieldNameToT]) ?=>
            '{
              $td.asInstanceOf[TransformerDefinition[
                From,
                To,
                FieldRelabelled[fieldNameFromT, fieldNameToT, Cfg],
                Flags
              ]]
          }
    }(selectorFrom, selectorTo)

  def withCoproductInstance[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Inst: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]],
      f: Expr[Inst => To]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    '{
      WithRuntimeDataStore
        .update($td, $f)
        .asInstanceOf[TransformerDefinition[From, To, CoproductInstance[Inst, To, Cfg], Flags]]
    }

  def withConstructorImpl[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      Ctor: Type
  ](
      ti: Expr[TransformerDefinition[From, To, Cfg, Flags]],
      f: Expr[Ctor]
  )(using Quotes): Expr[TransformerDefinition[From, To, ? <: TransformerCfg, Flags]] =
    DslMacroUtils().applyConstructorType {
      [ctor <: ArgumentLists] =>
        (_: Type[ctor]) ?=>
          '{
            WithRuntimeDataStore
              .update($ti, $f)
              .asInstanceOf[TransformerDefinition[From, To, Constructor[ctor, To, Cfg], Flags]]
        }
    }(f)

  def buildTransformer[
      From: Type,
      To: Type,
      Cfg <: TransformerCfg: Type,
      Flags <: TransformerFlags: Type,
      ImplicitScopeFlags <: TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]]
  )(using Quotes): Expr[Transformer[From, To]] =
    TransformerMacros.deriveTotalTransformerWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](td)
}
