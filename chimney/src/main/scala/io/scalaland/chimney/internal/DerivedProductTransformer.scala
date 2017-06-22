package io.scalaland.chimney.internal

import shapeless.labelled.{FieldType, field}
import shapeless.{::, HList, HNil}

trait DerivedProductTransformer[From, FromLG <: HList, ToLG <: HList, Modifiers <: HList] {

  def transform(src: FromLG, modifiers: Modifiers): ToLG
}

object DerivedProductTransformer extends ProductInstances {

  final def apply[From, FromLG <: HList, ToLG <: HList, Modifiers <: HList](
    implicit dpt: DerivedProductTransformer[From, FromLG, ToLG, Modifiers]
  ): DerivedProductTransformer[From, FromLG, ToLG, Modifiers] = dpt

  final def instance[From, FromLG <: HList, ToLG <: HList, Modifiers <: HList](
    f: (FromLG, Modifiers) => ToLG
  ): DerivedProductTransformer[From, FromLG, ToLG, Modifiers] =
    new DerivedProductTransformer[From, FromLG, ToLG, Modifiers] {
      @inline final def transform(src: FromLG, modifiers: Modifiers): ToLG = f(src, modifiers)
    }
}

trait ProductInstances {

  implicit final def hnilCase[From, FromLG <: HList, Modifiers <: HList]
    : DerivedProductTransformer[From, FromLG, HNil, Modifiers] =
    DerivedProductTransformer.instance { (_: FromLG, _: Modifiers) =>
      HNil
    }

  implicit final def hconsCase[From, FromLG <: HList, Label <: Symbol, HeadToT, TailToLG <: HList, Modifiers <: HList](
    implicit vp: ValueProvider[From, FromLG, HeadToT, Label, Modifiers],
    tailTransformer: DerivedProductTransformer[From, FromLG, TailToLG, Modifiers]
  ): DerivedProductTransformer[From, FromLG, FieldType[Label, HeadToT] :: TailToLG, Modifiers] =
    DerivedProductTransformer.instance { (src: FromLG, modifiers: Modifiers) =>
      field[Label](vp.provide(src, modifiers)) :: tailTransformer.transform(src, modifiers)
    }
}
