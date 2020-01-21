package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.{ChimneyBlackboxMacros, TransformerDefinitionWhiteboxMacros}

import scala.language.experimental.macros

final class TransformerDefinition[From, To, C <: TransformerCfg](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) {
  def disableDefaultValues: TransformerDefinition[From, To, DisableDefaultValues[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, DisableDefaultValues[C]]]

  def enableBeanGetters: TransformerDefinition[From, To, EnableBeanGetters[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableBeanGetters[C]]]

  def enableBeanSetters: TransformerDefinition[From, To, EnableBeanSetters[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableBeanSetters[C]]]

  def enableOptionDefaultsToNone: TransformerDefinition[From, To, EnableOptionDefaultsToNone[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableOptionDefaultsToNone[C]]]

  def enableUnsafeOption: TransformerDefinition[From, To, EnableUnsafeOption[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableUnsafeOption[C]]]

  def withFieldConst[T, U](selector: To => T, value: U): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withFieldConstImpl[From, To, T, U, C]

  def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C]

  def withFieldRenamed[T, U](selectorFrom: From => T, selectorTo: To => U): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withFieldRenamedImpl[From, To, T, U, C]

  def withCoproductInstance[Inst](f: Inst => To): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withCoproductInstanceImpl[From, To, Inst, C]

  def buildTransformer: Transformer[From, To] =
    macro ChimneyBlackboxMacros.buildTransformerImpl[From, To, C]
}
