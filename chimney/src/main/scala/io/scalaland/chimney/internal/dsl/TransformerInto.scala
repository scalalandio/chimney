package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.internal._

import scala.language.experimental.macros

final class TransformerInto[From, To, C <: Cfg](
    val source: From,
    val td: TransformerDefinition[From, To, C]
) {

  def disableDefaultValues: TransformerInto[From, To, DisableDefaultValues[C]] =
    new TransformerInto(source, td.disableDefaultValues)

  def enableBeanGetters: TransformerInto[From, To, EnableBeanGetters[C]] =
    new TransformerInto(source, td.enableBeanGetters)

  def enableBeanSetters: TransformerInto[From, To, EnableBeanSetters[C]] =
    new TransformerInto(source, td.enableBeanSetters)

  def enableOptionDefaultsToNone: TransformerInto[From, To, EnableOptionDefaultsToNone[C]] =
    new TransformerInto(source, td.enableOptionDefaultsToNone)

  def enableUnsafeOption: TransformerInto[From, To, EnableUnsafeOption[C]] =
    new TransformerInto(source, td.enableUnsafeOption)

  def withFieldConst[T, U](selector: To => T, value: U): TransformerInto[From, To, _] =
    macro ChimneyWhiteboxMacros.withFieldConstImpl2[From, To, T, U, C]

  def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerInto[From, To, _] =
    macro ChimneyWhiteboxMacros.withFieldComputedImpl2[From, To, T, U, C]

  def withFieldRenamed[T, U](selectorFrom: From => T, selectorTo: To => U): TransformerInto[From, To, _] =
    macro ChimneyWhiteboxMacros.withFieldRenamedImpl2[From, To, T, U, C]

  def withCoproductInstance[Inst](f: Inst => To): TransformerInto[From, To, _] =
    macro ChimneyWhiteboxMacros.withCoproductInstanceImpl2[From, To, Inst, C]

  def transform: To =
    macro ChimneyBlackboxMacros.transformImpl[From, To, C]
}
