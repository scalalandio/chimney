package io.scalaland.chimney.internal

import io.scalaland.chimney._
import shapeless.labelled._
import shapeless._

trait PatcherInstances {

  final def instance[T, P](f: (T, P) => T): Patcher[T, P] =
    new Patcher[T, P] {
      @inline final def patch(obj: T, patch: P): T = f(obj, patch)
    }

  implicit def hnilCase[TLG <: HList]: Patcher[TLG, HNil] =
    instance { (obj: TLG, _: HNil) =>
      obj
    }

  implicit def hconsCase[L <: Symbol, T, PTail <: HList, U, TLG <: HList](
    implicit sel: ops.record.Selector.Aux[TLG, L, U],
    dt: DerivedTransformer[T, U, HNil],
    upd: ops.record.Updater.Aux[TLG, FieldType[L, U], TLG],
    tailPatcher: Patcher[TLG, PTail]
  ): Patcher[TLG, FieldType[L, T] :: PTail] =
    instance { (obj: TLG, patch: FieldType[L, T] :: PTail) =>
      {
        val patchedHead = upd(obj, field[L](dt.transform(patch.head, HNil)))
        tailPatcher.patch(patchedHead, patch.tail)
      }
    }

  implicit def gen[T, P, TLG, PLG](implicit tlg: LabelledGeneric.Aux[T, TLG],
                                   plg: LabelledGeneric.Aux[P, PLG],
                                   patcher: Patcher[TLG, PLG]): Patcher[T, P] =
    instance { (obj: T, patch: P) =>
      tlg.from(patcher.patch(tlg.to(obj), plg.to(patch)))
    }
}
