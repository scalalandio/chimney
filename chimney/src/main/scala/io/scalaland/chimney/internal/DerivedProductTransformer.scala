package io.scalaland.chimney.internal

import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.FilterNot
import shapeless.ops.record.Selector

trait DerivedProductTransformer[From, FromLG <: HList, To, ToLG <: HList, Modifiers <: HList] {

  def transform(src: FromLG, modifiers: Modifiers): ToLG
}

object DerivedProductTransformer extends ProductInstances {

  final def apply[From, FromLG <: HList, To, ToLG <: HList, Modifiers <: HList](
    implicit dpt: DerivedProductTransformer[From, FromLG, To, ToLG, Modifiers]
  ): DerivedProductTransformer[From, FromLG, To, ToLG, Modifiers] = dpt
}

trait ProductInstances extends LowPriorityProductInstances {

  implicit final def hnilCase[From, FromLG <: HList, To, Modifiers <: HList]
    : DerivedProductTransformer[From, FromLG, To, HNil, Modifiers] =
    (_: FromLG, _: Modifiers) => HNil

  implicit final def hconsCase[From,
                               FromLG <: HList,
                               To,
                               Label <: Symbol,
                               HeadToT,
                               TailToLG <: HList,
                               Modifiers <: HList](
    implicit vp: ValueProvider[From, FromLG, HeadToT, Label, Modifiers],
    tailTransformer: DerivedProductTransformer[From, FromLG, To, TailToLG, Modifiers]
  ): DerivedProductTransformer[From, FromLG, To, FieldType[Label, HeadToT] :: TailToLG, Modifiers] =
    (src: FromLG, modifiers: Modifiers) =>
      field[Label](vp.provide(src, modifiers)) :: tailTransformer.transform(src, modifiers)
}

trait LowPriorityProductInstances {
  implicit final def hconsCaseDefault[From,
                                      FromLG <: HList,
                                      To,
                                      Label <: Symbol,
                                      HeadToT,
                                      TailToLG <: HList,
                                      Defaults <: HList,
                                      Modifiers <: HList](
    implicit
    notDisabled: FilterNot.Aux[Modifiers, Modifier.disableDefaultValues, Modifiers],
    defaults: Default.AsRecord.Aux[To, Defaults],
    defaultSelector: Selector.Aux[Defaults, Label, HeadToT],
    tailTransformer: DerivedProductTransformer[From, FromLG, To, TailToLG, Modifiers]
  ): DerivedProductTransformer[From, FromLG, To, FieldType[Label, HeadToT] :: TailToLG, Modifiers] =
    (src: FromLG, modifiers: Modifiers) =>
      field[Label](defaultSelector(defaults())) :: tailTransformer.transform(src, modifiers)
}
