package io.scalaland.chimney

import shapeless.{Coproduct, HList, Witness, ops}
import shapeless.labelled.{field, FieldType}

trait CoproductInstanceProvider[Label <: Symbol, FromT, ToLG <: Coproduct, Modifiers <: HList] {

  def provide(src: FieldType[Label, FromT], modifiers: Modifiers): ToLG
}

object CoproductInstanceProvider {

  implicit final def matchingObjCase[ToLG <: Coproduct, Label <: Symbol, FromT, TargetT, Modifiers <: HList](
    implicit sel: ops.union.Selector.Aux[ToLG, Label, TargetT],
    wit: Witness.Aux[TargetT],
    inj: ops.coproduct.Inject[ToLG, FieldType[Label, TargetT]]
  ): CoproductInstanceProvider[Label, FromT, ToLG, Modifiers] =
    (_: FieldType[Label, FromT], _: Modifiers) => inj(field[Label](wit.value))
}
