package io.scalaland.chimney.internal

import shapeless.labelled.FieldType
import shapeless.{:+:, CNil, Coproduct, HList, Inl, Inr}

trait DerivedCoproductTransformer[From, FromLG <: Coproduct, ToLG <: Coproduct, Modifiers <: HList] {

  def transform(src: FromLG, modifiers: Modifiers): ToLG
}

object DerivedCoproductTransformer extends CoproductInstances {

  final def apply[From, FromLG <: Coproduct, ToLG <: Coproduct, Modifiers <: HList](
    implicit dct: DerivedCoproductTransformer[From, FromLG, ToLG, Modifiers]
  ): DerivedCoproductTransformer[From, FromLG, ToLG, Modifiers] = dct
}

trait CoproductInstances {

  // $COVERAGE-OFF$
  implicit final def cnilCase[From, ToLG <: Coproduct, Modifiers <: HList]
    : DerivedCoproductTransformer[From, CNil, ToLG, Modifiers] =
    (_: CNil, _: Modifiers) => null.asInstanceOf[ToLG]
  // $COVERAGE-ON$

  implicit final def coproductCase[From,
                                   TailFromLG <: Coproduct,
                                   Label <: Symbol,
                                   HeadToT,
                                   ToLG <: Coproduct,
                                   Modifiers <: HList](
    implicit cip: CoproductInstanceProvider[Label, HeadToT, ToLG, Modifiers],
    tailTransformer: DerivedCoproductTransformer[From, TailFromLG, ToLG, Modifiers]
  ): DerivedCoproductTransformer[From, FieldType[Label, HeadToT] :+: TailFromLG, ToLG, Modifiers] =
    (src: FieldType[Label, HeadToT] :+: TailFromLG, modifiers: Modifiers) =>
      src match {
        case Inl(head) => cip.provide(head, modifiers)
        case Inr(tail) => tailTransformer.transform(tail, modifiers)
    }
}
