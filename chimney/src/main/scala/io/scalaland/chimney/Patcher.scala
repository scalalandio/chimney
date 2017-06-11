package io.scalaland.chimney

import shapeless.labelled._
import shapeless._

trait Patcher[T, P] {
  def patch(obj: T, patch: P): T
}

object Patcher {

  implicit def hnilCase[TLG <: HList]: Patcher[TLG, HNil] =
    (obj: TLG, _: HNil) => obj

  implicit def hconsCase[L <: Symbol, T, PTail <: HList, U, TLG <: HList](
    implicit sel: ops.record.Selector.Aux[TLG, L, U],
    dt: DerivedTransformer[T, U, HNil],
    upd: ops.record.Updater.Aux[TLG, FieldType[L, U], TLG],
    tailPatcher: Patcher[TLG, PTail]
  ): Patcher[TLG, FieldType[L, T] :: PTail] =
    (obj: TLG, patch: FieldType[L, T] :: PTail) => {
      val patchedHead = upd(obj, field[L](dt.transform(patch.head, HNil)))
      tailPatcher.patch(patchedHead, patch.tail)
    }

  implicit def gen[T, P, TLG, PLG](implicit tlg: LabelledGeneric.Aux[T, TLG],
                                   plg: LabelledGeneric.Aux[P, PLG],
                                   patcher: Patcher[TLG, PLG]): Patcher[T, P] =
    (obj: T, patch: P) => tlg.from(patcher.patch(tlg.to(obj), plg.to(patch)))

}
