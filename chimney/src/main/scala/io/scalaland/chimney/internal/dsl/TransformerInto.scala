package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.internal._

import scala.language.experimental.macros

final class TransformerInto[From, To, C <: Cfg](
    val source: From,
    val td: TransformerDefinition[From, To, C]
    //      val overrides: Map[String, Any],
    //      val instances: Map[(String, String), Any]
) {

  def disableDefaultValues: TransformerInto[From, To, DisableDefaultValues[C]] =
    new TransformerInto(source, td.disableDefaultValues)
  //      new TransformerInto[From, To, DisableDefaultValues[C]](source, overrides, instances)

  def enableBeanGetters: TransformerInto[From, To, EnableBeanGetters[C]] =
    new TransformerInto(source, td.enableBeanGetters)
  //      new TransformerInto[From, To, EnableBeanGetters[C]](source, overrides, instances)

  def enableBeanSetters: TransformerInto[From, To, EnableBeanSetters[C]] =
    new TransformerInto(source, td.enableBeanSetters)
  //      new TransformerInto[From, To, EnableBeanSetters[C]](source, overrides, instances)

  def enableOptionDefaultsToNone: TransformerInto[From, To, EnableOptionDefaultsToNone[C]] =
    new TransformerInto(source, td.enableOptionDefaultsToNone)
  //    new TransformerInto[From, To, EnableOptionDefaultsToNone[C]](source, overrides, instances)

  def enableUnsafeOption: TransformerInto[From, To, EnableUnsafeOption[C]] =
    new TransformerInto(source, td.enableUnsafeOption)
  //    new TransformerInto[From, To, EnableUnsafeOption[C]](source, overrides, instances)

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
