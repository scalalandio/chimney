package io.scalaland.chimney

import shapeless._
import shapeless.labelled.FieldType

trait DerivedCoproductTransformer[From, FromG <: Coproduct, ToG <: Coproduct, Modifiers <: HList] {

  def transform(src: FromG, modifiers: Modifiers): ToG
}

object DerivedCoproductTransformer extends CoproductInstances {

  def apply[From, FromG <: Coproduct, ToG <: Coproduct, Modifiers <: HList](implicit dt: DerivedCoproductTransformer[From, FromG, ToG, Modifiers]): DerivedCoproductTransformer[From, FromG, ToG, Modifiers] = dt
}

trait CoproductInstances {

  // $COVERAGE-OFF$
  implicit def cnilCase[From, ToG <: Coproduct, Modifiers <: HList]
  : DerivedCoproductTransformer[From, CNil, ToG, Modifiers] =
    (_: CNil, _: Modifiers) => null.asInstanceOf[ToG]
  // $COVERAGE-ON$

  implicit def coproductCase[From, TailFromG <: Coproduct, Label <: Symbol, HeadT, ToG <: Coproduct, Modifiers <: HList]
  (implicit cip: CoproductInstanceProvider[ToG, Label, HeadT, Modifiers],
   tailTransformer: DerivedCoproductTransformer[From, TailFromG, ToG, Modifiers])
  : DerivedCoproductTransformer[From, FieldType[Label, HeadT] :+: TailFromG, ToG, Modifiers] =
    (src: FieldType[Label, HeadT] :+: TailFromG, modifiers: Modifiers) =>
      (src: FieldType[Label, HeadT] :+: TailFromG) match {
        case Inl(hd) => cip.provide(hd, modifiers)
        case Inr(tl) => tailTransformer.transform(tl, modifiers)
      }
}
