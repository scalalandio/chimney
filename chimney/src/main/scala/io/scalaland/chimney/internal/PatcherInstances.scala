package io.scalaland.chimney.internal

import io.scalaland.chimney._
import shapeless.labelled._
import shapeless._

trait PatcherInstances extends PatcherHConstInstance {

  implicit def hnilCase[TLG <: HList]: Patcher[TLG, HNil] =
    (obj: TLG, _: HNil) => obj
}

trait PatcherHConstInstance extends PatcherOptionalHConsInstance {

  implicit def hconsCase[L <: Symbol, T, PTail <: HList, U, TLG <: HList](
    implicit sel: ops.record.Selector.Aux[TLG, L, U],
    dt: Transformer[T, U],
    upd: ops.record.Updater.Aux[TLG, FieldType[L, U], TLG],
    tailPatcher: Patcher[TLG, PTail]
  ): Patcher[TLG, FieldType[L, T] :: PTail] =
    (obj: TLG, patch: FieldType[L, T] :: PTail) => {
      val patchedHead = upd(obj, field[L](dt.transform(patch.head)))
      tailPatcher.patch(patchedHead, patch.tail)
    }
}

trait PatcherOptionalHConsInstance extends PatcherGenericInstance {

  implicit def optionalHconsCase[L <: Symbol, T, PTail <: HList, U, TLG <: HList](
    implicit sel: ops.record.Selector.Aux[TLG, L, U],
    dt: DerivedTransformer[T, U, HNil],
    upd: ops.record.Updater.Aux[TLG, FieldType[L, U], TLG],
    tailPatcher: Patcher[TLG, PTail]
  ): Patcher[TLG, FieldType[L, Option[T]] :: PTail] =
    (obj: TLG, patch: FieldType[L, Option[T]] :: PTail) =>
      (patch.head: Option[T]) match {
        case Some(patchedValue) =>
          val patchedHead = upd(obj, field[L](dt.transform(patchedValue, HNil)))
          tailPatcher.patch(patchedHead, patch.tail)
        case None =>
          tailPatcher.patch(obj, patch.tail)
    }
}

trait PatcherGenericInstance {

  implicit def gen[T, P, TLG, PLG](implicit tlg: LabelledGeneric.Aux[T, TLG],
                                   plg: LabelledGeneric.Aux[P, PLG],
                                   patcher: Patcher[TLG, PLG]): Patcher[T, P] =
    (obj: T, patch: P) => tlg.from(patcher.patch(tlg.to(obj), plg.to(patch)))
}
