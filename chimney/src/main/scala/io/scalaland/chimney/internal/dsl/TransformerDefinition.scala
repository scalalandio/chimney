package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal._
import scala.language.experimental.macros

final class TransformerDefinition[From, To, C <: Cfg](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) {
  def disableDefaultValues: TransformerDefinition[From, To, DisableDefaultValues[C]] =
    new TransformerDefinition[From, To, DisableDefaultValues[C]](overrides, instances)

  def enableBeanGetters: TransformerDefinition[From, To, EnableBeanGetters[C]] =
    new TransformerDefinition[From, To, EnableBeanGetters[C]](overrides, instances)

  def enableBeanSetters: TransformerDefinition[From, To, EnableBeanSetters[C]] =
    new TransformerDefinition[From, To, EnableBeanSetters[C]](overrides, instances)

  def enableOptionDefaultsToNone: TransformerDefinition[From, To, EnableOptionDefaultsToNone[C]] =
    new TransformerDefinition[From, To, EnableOptionDefaultsToNone[C]](overrides, instances)

  def enableUnsafeOption: TransformerDefinition[From, To, EnableUnsafeOption[C]] =
    new TransformerDefinition[From, To, EnableUnsafeOption[C]](overrides, instances)

  def withFieldConst[T, U](selector: To => T, value: U): TransformerDefinition[From, To, _] =
    macro ChimneyWhiteboxMacros.withFieldConstImpl[From, To, T, U, C]

  def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerDefinition[From, To, _] =
    macro ChimneyWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C]

  def withFieldRenamed[T, U](selectorFrom: From => T, selectorTo: To => U): TransformerDefinition[From, To, _] =
    macro ChimneyWhiteboxMacros.withFieldRenamedImpl[From, To, T, U, C]

  def withCoproductInstance[Inst](f: Inst => To): TransformerDefinition[From, To, _] =
    macro ChimneyWhiteboxMacros.withCoproductInstanceImpl[From, To, Inst, C]

  def buildTransformer: Transformer[From, To] =
    macro ChimneyBlackboxMacros.buildTransformerImpl[From, To, C]
}
