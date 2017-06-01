package io.scalaland.chimney

import shapeless._
import shapeless.labelled._

trait DerivedProductTransformer[From, FromG <: HList, ToG <: HList, Modifiers <: HList] {

  def transform(src: FromG, modifiers: Modifiers): ToG
}

object DerivedProductTransformer extends ProductInstances {

  def apply[From, FromG <: HList, ToG <: HList, Modifiers <: HList](implicit dt: DerivedProductTransformer[From, FromG, ToG, Modifiers]): DerivedProductTransformer[From, FromG, ToG, Modifiers] = dt
}

trait ProductInstances {

  implicit def hnilCase[From, FromG <: HList, Modifiers <: HList]
  : DerivedProductTransformer[From, FromG, HNil, Modifiers] =
    (_: FromG, _: Modifiers) => HNil

  implicit def hconsCase[From, FromG <: HList, Label <: Symbol, ToFieldT, TailToG <: HList, Modifiers <: HList]
  (implicit vp: ValueProvider[From, FromG, ToFieldT, Label, Modifiers],
   tailTransformer: DerivedProductTransformer[From, FromG, TailToG, Modifiers])
  : DerivedProductTransformer[From, FromG, FieldType[Label, ToFieldT] :: TailToG, Modifiers] = {
    (src: FromG, modifiers: Modifiers) =>
      field[Label](vp.provide(src, modifiers)) :: tailTransformer.transform(src, modifiers)
  }
}

