package io.scalaland.chimney

import shapeless.Witness


sealed trait Modifier

object Modifier {

  class fieldFunction[L <: Symbol, From, T](val f: From => T)
    extends Modifier

  class relabel[L1 <: Symbol, L2 <: Symbol]
    extends Modifier


  def fieldConstant[From, T]
    (label: Witness.Lt[Symbol], value: T): fieldFunction[label.T, From, T] =
    new fieldFunction[label.T, From, T]((_: From) => value)

  def fieldFunction[From, T]
    (label: Witness.Lt[Symbol], f: From => T): fieldFunction[label.T, From, T] =
    new fieldFunction[label.T, From, T](f)

  def relabel(label1: Witness.Lt[Symbol],
              label2: Witness.Lt[Symbol]): relabel[label1.T, label2.T] =
    new relabel
}
