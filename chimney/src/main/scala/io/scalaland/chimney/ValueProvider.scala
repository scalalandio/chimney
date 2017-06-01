package io.scalaland.chimney

import shapeless.{::, HList, HNil, LabelledGeneric, Witness, ops}

trait ValueProvider[FromG, From, T, Label <: Symbol, Modifiers <: HList] {
  def provide(from: FromG, modifiers: Modifiers): T
}

object ValueProvider extends ValueProviderDerivation {

  def provide[From, FromG <: HList, TargetT, Modifiers <: HList]
    (from: From, targetLabel: Witness.Lt[Symbol], clz: Class[TargetT], modifiers: Modifiers)
    (implicit lg: LabelledGeneric.Aux[From, FromG],
     vp: ValueProvider[FromG, From, TargetT, targetLabel.T, Modifiers])
  : TargetT = vp.provide(lg.to(from), modifiers)

}

trait ValueProviderDerivation {

  implicit def hnilCase[Modifiers <: HNil, FromG <: HList, From, FromT, ToT, L <: Symbol]
  (implicit fieldSelector: ops.record.Selector.Aux[FromG, L, FromT],
   fieldTransformer: DerivedTransformer[FromT, ToT, Modifiers])
  : ValueProvider[FromG, From, ToT, L, Modifiers] =
    (from: FromG, modifiers: Modifiers) =>
      fieldTransformer.transform(fieldSelector(from), modifiers)

  implicit def hconsFieldFunctionCase[FromG <: HList, From, T, L <: Symbol, Modifiers <: HList]
  (implicit fromLG: LabelledGeneric.Aux[From, FromG])
  : ValueProvider[FromG, From, T, L, Modifier.fieldFunction[L, From, T] :: Modifiers] =
    (from: FromG, modifiers: Modifier.fieldFunction[L, From, T] :: Modifiers) =>
      modifiers.head.f(fromLG.from(from))

  implicit def hconsRelabelCase[FromG <: HList, From, T, LFrom <: Symbol, L <: Symbol, Modifiers <: HList]
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
