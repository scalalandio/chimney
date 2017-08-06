package io.scalaland.chimney.internal

import shapeless.labelled.{FieldType, field}
import shapeless.{::, HList, HNil}
import samurai._

trait DerivedProductTransformer[From, FromLG <: HList, ToLG <: HList, Modifiers <: HList] {

  def transform(src: FromLG, modifiers: Modifiers): ToLG
}

object DerivedProductTransformer extends ProductInstances {

  final def apply[From, FromLG <: HList, ToLG <: HList, Modifiers <: HList](
    implicit dpt: DerivedProductTransformer[From, FromLG, ToLG, Modifiers]
  ): DerivedProductTransformer[From, FromLG, ToLG, Modifiers] = dpt
}

trait ProductInstances {

  @sam implicit final def hnilCase[From, FromLG <: HList, Modifiers <: HList]
    : DerivedProductTransformer[From, FromLG, HNil, Modifiers] =
    (_: FromLG, _: Modifiers) => HNil

  @sam implicit final def hconsCase[From, FromLG <: HList, Label <: Symbol, HeadToT, TailToLG <: HList, Modifiers <: HList](
    implicit vp: ValueProvider[From, FromLG, HeadToT, Label, Modifiers],
    tailTransformer: DerivedProductTransformer[From, FromLG, TailToLG, Modifiers]
  ): DerivedProductTransformer[From, FromLG, FieldType[Label, HeadToT] :: TailToLG, Modifiers] =
    (src: FromLG, modifiers: Modifiers) =>
      field[Label](vp.provide(src, modifiers)) :: tailTransformer.transform(src, modifiers)
}

