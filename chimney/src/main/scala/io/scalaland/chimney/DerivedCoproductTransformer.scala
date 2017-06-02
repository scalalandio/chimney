package io.scalaland.chimney

import shapeless.{ :+:, Coproduct, CNil, HList, Inl, Inr }
import shapeless.labelled.FieldType

/** Automatically derived type-call for mapping one [[shapeless.Coproduct]] into another.
  *
  * It is an intermediate representation for Coproduct types that is intended for [[DerivedTransformer]] to use.
  *
  * @tparam From original non-generic type: sealed trait, etc.
  * @tparam FromLG generic representation of [[From]]
  * @tparam ToLG generic representation of a target type
  * @tparam Modifiers list of modifiers that will be traversed before any attempt to obtain values the default way
  */
trait DerivedCoproductTransformer[From, FromLG <: Coproduct, ToLG <: Coproduct, Modifiers <: HList] {

  /** Transforms generic representation of original value into generic representation of a target type.
    *
    * @param src generic representation of original value
    * @param modifiers list of modifiers matching [[Modifiers]] type
    * @return generic representation of a target type
    */
  def transform(src: FromLG, modifiers: Modifiers): ToLG
}

/** Utilities and instances for [[DerivedCoproductTransformer]]. */
object DerivedCoproductTransformer extends CoproductInstances {

  /** Returns an instance for given parameters.
    *
    * @param dct implicit instance
    * @tparam From original non-generic type: sealed trait, etc.
    * @tparam FromLG generic representation of [[From]]
    * @tparam ToLG generic representation of a target type
    * @tparam Modifiers list of modifiers that will be traversed before any attempt to obtain values the default way
    */
  final def apply[From, FromLG <: Coproduct, ToLG <: Coproduct, Modifiers <: HList](
    implicit
    dct: DerivedCoproductTransformer[From, FromLG, ToLG, Modifiers]
  ): DerivedCoproductTransformer[From, FromLG, ToLG, Modifiers] = dct
}

trait CoproductInstances {

  // $COVERAGE-OFF$
  implicit final def cnilCase[From, ToLG <: Coproduct, Modifiers <: HList]: DerivedCoproductTransformer[From, CNil, ToLG, Modifiers] =
    (_: CNil, _: Modifiers) => null.asInstanceOf[ToLG]
  // $COVERAGE-ON$

  implicit final def coproductCase[From, TailFromLG <: Coproduct, Label <: Symbol, HeadToT, ToLG <: Coproduct, Modifiers <: HList](
    implicit
    cip: CoproductInstanceProvider[Label, HeadToT, ToLG, Modifiers],
    tailTransformer: DerivedCoproductTransformer[From, TailFromLG, ToLG, Modifiers]
  ): DerivedCoproductTransformer[From, FieldType[Label, HeadToT] :+: TailFromLG, ToLG, Modifiers] =
    (src: FieldType[Label, HeadToT] :+: TailFromLG, modifiers: Modifiers) => src match {
      case Inl(head) => cip.provide(head, modifiers)
      case Inr(tail) => tailTransformer.transform(tail, modifiers)
    }
}
