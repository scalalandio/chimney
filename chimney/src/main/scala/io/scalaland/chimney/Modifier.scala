package io.scalaland.chimney

import shapeless.Witness


sealed trait Modifier
class FieldFunctionModifier[L <: Symbol, From, T](val f: From => T)
  extends Modifier

class RelabelModifier[L1 <: Symbol, L2 <: Symbol]
  extends Modifier

object Modifier {

  def fieldConstant[From, T]
    (label: Witness.Lt[Symbol], value: T): FieldFunctionModifier[label.T, From, T] =
    new FieldFunctionModifier((_: From) => value)

  def fieldFunction[From, T]
    (label: Witness.Lt[Symbol], f: From => T): FieldFunctionModifier[label.T, From, T] =
    new FieldFunctionModifier(f)

  def relabel(label1: Witness.Lt[Symbol],
              label2: Witness.Lt[Symbol]): RelabelModifier[label1.T, label2.T] =
    new RelabelModifier
}
