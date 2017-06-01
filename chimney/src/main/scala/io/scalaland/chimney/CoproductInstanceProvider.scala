package io.scalaland.chimney

import shapeless.labelled.{FieldType, field}
import shapeless.{Coproduct, HList, Witness, ops}

trait CoproductInstanceProvider[ToG <: Coproduct, Label <: Symbol, T, Modifiers <: HList] {
  def provide(srcInstance: FieldType[Label, T], modifiers: Modifiers): ToG
}

object CoproductInstanceProvider {

  implicit def matchingObjCase[ToG <: Coproduct, ToHList <: HList, Label <: Symbol, T, TargetT, Modifiers <: HList]
  (implicit sel: ops.union.Selector.Aux[ToG, Label, TargetT],
   wit: Witness.Aux[TargetT],
   inj: ops.coproduct.Inject[ToG, FieldType[Label, TargetT]])
  : CoproductInstanceProvider[ToG, Label, T, Modifiers] =
    (_: FieldType[Label, T], _: Modifiers) =>
      inj(field[Label](wit.value))
}
