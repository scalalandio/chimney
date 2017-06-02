package io.scalaland.chimney

import shapeless.{ ::, HList, HNil }
import shapeless.labelled.{ field, FieldType }

/** Automatically derived type-call for mapping one [[shapeless.HList]] into another.
  *
  * It is an intermediate representation for Product types that is intended for [[DerivedTransformer]] to use.
  *
  * @tparam From original non-generic type: case class, tuple, etc.
  * @tparam FromLG generic representation of [[From]]
  * @tparam ToLG generic representation of a target type
  * @tparam Modifiers list of modifiers that will be traversed before any attempt to obtain values the default way
  */
trait DerivedProductTransformer[From, FromLG <: HList, ToLG <: HList, Modifiers <: HList] {

  /** Transforms generic representation of original value into generic representation of a target type.
    *
    * @param src generic representation of original value
    * @param modifiers list of modifiers matching [[Modifiers]] type
    * @return generic representation of a target type
    */
  def transform(src: FromLG, modifiers: Modifiers): ToLG
}

/** Utilities and instances for [[DerivedProductTransformer]]. */
object DerivedProductTransformer extends ProductInstances {

  /** Returns an instance for given parameters.
    *
    * @param dpt implicit instance
    * @tparam From original non-generic type: sealed trait, etc.
    * @tparam FromLG generic representation of [[From]]
    * @tparam ToLG generic representation of a target type
    * @tparam Modifiers list of modifiers that will be traversed before any attempt to obtain values the default way
    */
  final def apply[From, FromLG <: HList, ToLG <: HList, Modifiers <: HList](
    implicit
    dpt: DerivedProductTransformer[From, FromLG, ToLG, Modifiers]
  ): DerivedProductTransformer[From, FromLG, ToLG, Modifiers] = dpt
}

trait ProductInstances {

  implicit final def hnilCase[From, FromLG <: HList, Modifiers <: HList]: DerivedProductTransformer[From, FromLG, HNil, Modifiers] =
    (_: FromLG, _: Modifiers) => HNil

  implicit final def hconsCase[From, FromLG <: HList, Label <: Symbol, HeadToT, TailToLG <: HList, Modifiers <: HList](
    implicit
    vp: ValueProvider[From, FromLG, HeadToT, Label, Modifiers],
    tailTransformer: DerivedProductTransformer[From, FromLG, TailToLG, Modifiers]
  ): DerivedProductTransformer[From, FromLG, FieldType[Label, HeadToT] :: TailToLG, Modifiers] =
    (src: FromLG, modifiers: Modifiers) =>
      field[Label](vp.provide(src, modifiers)) :: tailTransformer.transform(src, modifiers)
}
