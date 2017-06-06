package io.scalaland.chimney

import shapeless.Witness

sealed trait Modifier

object Modifier {

  private[chimney] class fieldFunction[Label <: Symbol, From, T](val map: From => T) extends Modifier
  private[chimney] class relabel[FromLabel <: Symbol, ToLabel <: Symbol] extends Modifier
  private[chimney] class coproductInstance[From, T](val f: From => T) extends Modifier

  final def fieldConstant[From, T](label: Witness.Lt[Symbol], value: T): fieldFunction[label.T, From, T] =
    new fieldFunction[label.T, From, T]((_: From) => value)

  final def fieldFunction[From, T](label: Witness.Lt[Symbol], map: From => T): fieldFunction[label.T, From, T] =
    new fieldFunction[label.T, From, T](map)

  final def relabel(label1: Witness.Lt[Symbol], label2: Witness.Lt[Symbol]): relabel[label1.T, label2.T] =
    new relabel

  final def coproductInstance[From, T](convert: From => T): coproductInstance[From, T] =
    new coproductInstance(convert)
}
