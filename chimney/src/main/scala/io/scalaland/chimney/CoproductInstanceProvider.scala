package io.scalaland.chimney

import shapeless.{ Coproduct, HList, Witness, ops }
import shapeless.labelled.{ field, FieldType }

/** Provides a value of a Coprodict type.
  *
  * @tparam Label field name used in original Product type
  * @tparam FromT non-generic representation of Coproduct type: sealed trait, etc.
  * @tparam ToLG generic representation of a target type
  * @tparam Modifiers list of modifiers that will be traversed before any attempt to obtain values the default way
  */
trait CoproductInstanceProvider[Label <: Symbol, FromT, ToLG <: Coproduct, Modifiers <: HList] {

  /** Extracts target Coproduct value from original field.
    *
    * @param src field value labelled with field's name
    * @param modifiers list of modifiers matching [[Modifiers]] type
    * @return extracted Coproduct value
    */
  def provide(src: FieldType[Label, FromT], modifiers: Modifiers): ToLG
}

object CoproductInstanceProvider {

  implicit final def matchingObjCase[ToLG <: Coproduct, Label <: Symbol, FromT, TargetT, Modifiers <: HList](
    implicit
    sel: ops.union.Selector.Aux[ToLG, Label, TargetT],
    wit: Witness.Aux[TargetT],
    inj: ops.coproduct.Inject[ToLG, FieldType[Label, TargetT]]
  ): CoproductInstanceProvider[Label, FromT, ToLG, Modifiers] =
    (_: FieldType[Label, FromT], _: Modifiers) => inj(field[Label](wit.value))
}
