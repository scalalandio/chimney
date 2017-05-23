package io.scalaland.chimney

import shapeless.{::, HList, HNil, LabelledGeneric, ops}

trait ValueProvider[FromG, From, T, Label <: Symbol, Modifiers <: HList] {
  def provide(from: FromG, modifiers: Modifiers): T
}

object ValueProvider {

  implicit def hnilCase[FromG <: HList, From, FromT, ToT, L <: Symbol]
    (implicit fieldSelector: ops.record.Selector.Aux[FromG, L, FromT],
     fieldTransformer: DerivedTransformer[FromT, ToT, HNil])
  : ValueProvider[FromG, From, ToT, L, HNil] =
    (from: FromG, _: HNil) => fieldTransformer.transform(fieldSelector(from), HNil)

  implicit def hconsFieldFunctionCase[FromG <: HList, From, T, L <: Symbol, Modifiers <: HNil]
    (implicit fromLG: LabelledGeneric.Aux[From, FromG])
  : ValueProvider[FromG, From, T, L, Modifier.fieldFunction[L, From, T] :: Modifiers] =
    (from: FromG, modifiers: Modifier.fieldFunction[L, From, T] :: Modifiers) =>
      modifiers.head.f(fromLG.from(from))

  implicit def hconsRelabelCase[FromG <: HList, From, T, LFrom <: Symbol, L <: Symbol, Modifiers <: HNil]
    (implicit fieldSelector: ops.record.Selector.Aux[FromG, LFrom, T])
  : ValueProvider[FromG, From, T, L, Modifier.relabel[LFrom, L] :: Modifiers] =
    (from: FromG, _: Modifier.relabel[LFrom, L] :: Modifiers) =>
      fieldSelector(from)

  implicit def hconsTailCase[FromG <: HList, From, T, L <: Symbol, M <: Modifier, Ms <: HList]
    (implicit tvp: ValueProvider[FromG, From, T, L, Ms])
  : ValueProvider[FromG, From, T, L, M :: Ms] =
    (from: FromG, modifiers: M :: Ms) =>
      tvp.provide(from, modifiers.tail)

}
