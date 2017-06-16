package io.scalaland.chimney.internal

import io.scalaland.chimney.DerivedTransformer
import shapeless.{::, HList, Witness}

final class TransformerInto[From, To, Modifiers <: HList](val source: From, val modifiers: Modifiers) {

  def withFieldConst[T](label: Witness.Lt[Symbol],
                        value: T): TransformerInto[From, To, Modifier.fieldFunction[label.T, From, T] :: Modifiers] =
    withFieldComputed(label, _ => value)

  def withFieldComputed[T](
    label: Witness.Lt[Symbol],
    map: From => T
  ): TransformerInto[From, To, Modifier.fieldFunction[label.T, From, T] :: Modifiers] =
    new TransformerInto(source, new Modifier.fieldFunction[label.T, From, T](map) :: modifiers)

  def withFieldRenamed(
    labelFrom: Witness.Lt[Symbol],
    labelTo: Witness.Lt[Symbol]
  ): TransformerInto[From, To, Modifier.relabel[labelFrom.T, labelTo.T] :: Modifiers] =
    new TransformerInto(source, new Modifier.relabel[labelFrom.T, labelTo.T] :: modifiers)

  def withCoproductInstance[Inst](
    f: Inst => To
  ): TransformerInto[From, To, Modifier.coproductInstance[Inst, To] :: Modifiers] =
    new TransformerInto(source, new Modifier.coproductInstance[Inst, To](f) :: modifiers)

  def transform(implicit transformer: DerivedTransformer[From, To, Modifiers]): To =
    transformer.transform(source, modifiers)
}
