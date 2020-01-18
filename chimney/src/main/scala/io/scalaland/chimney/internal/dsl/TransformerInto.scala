package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.{ChimneyBlackboxMacros, TransformerIntoWhiteboxMacros}

import scala.language.experimental.macros

final class TransformerInto[From, To, C <: TransformerCfg](
    val source: From,
    val td: TransformerDefinition[From, To, C]
) {

  def disableDefaultValues: TransformerInto[From, To, DisableDefaultValues[C]] =
    this.asInstanceOf[TransformerInto[From, To, DisableDefaultValues[C]]]

  def enableBeanGetters: TransformerInto[From, To, EnableBeanGetters[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableBeanGetters[C]]]

  def enableBeanSetters: TransformerInto[From, To, EnableBeanSetters[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableBeanSetters[C]]]

  def enableOptionDefaultsToNone: TransformerInto[From, To, EnableOptionDefaultsToNone[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableOptionDefaultsToNone[C]]]

  def enableUnsafeOption: TransformerInto[From, To, EnableUnsafeOption[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableUnsafeOption[C]]]

  def withFieldConst[T, U](selector: To => T, value: U): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withFieldConstImpl[From, To, T, U, C]

  def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C]

  def withFieldRenamed[T, U](selectorFrom: From => T, selectorTo: To => U): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withFieldRenamedImpl[From, To, T, U, C]

  def withCoproductInstance[Inst](f: Inst => To): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withCoproductInstanceImpl[From, To, Inst, C]

  def transform: To =
    macro ChimneyBlackboxMacros.transformImpl[From, To, C]
}
