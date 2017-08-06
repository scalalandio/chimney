package io.scalaland.chimney.internal

import io.scalaland.chimney._
import shapeless.labelled._
import shapeless._
import samurai._

trait PatcherInstances {

  @sam implicit def hnilCase[TLG <: HList]: Patcher[TLG, HNil] =
    (obj: TLG, _: HNil) => obj

  @sam implicit def hconsCase[L <: Symbol, T, PTail <: HList, U, TLG <: HList](
    implicit sel: ops.record.Selector.Aux[TLG, L, U],
    dt: DerivedTransformer[T, U, HNil],
    upd: ops.record.Updater.Aux[TLG, FieldType[L, U], TLG],
    tailPatcher: Patcher[TLG, PTail]
  ): Patcher[TLG, FieldType[L, T] :: PTail] =
    (obj: TLG, patch: FieldType[L, T] :: PTail) => {
      val patchedHead = upd(obj, field[L](dt.transform(patch.head, HNil)))
      tailPatcher.patch(patchedHead, patch.tail)
    }

  @sam implicit def gen[T, P, TLG, PLG](implicit tlg: LabelledGeneric.Aux[T, TLG],
                                        plg: LabelledGeneric.Aux[P, PLG],
                                        patcher: Patcher[TLG, PLG]): Patcher[T, P] =
    (obj: T, patch: P) => tlg.from(patcher.patch(tlg.to(obj), plg.to(patch)))
}
