package io.scalaland.chimney

import shapeless.{::, HList, HNil, LabelledGeneric, Witness, ops}

trait ValueProvider[From, FromG, TargetT, Label <: Symbol, Modifiers <: HList] {
  def provide(from: FromG, modifiers: Modifiers): TargetT
}

object ValueProvider extends ValueProviderDerivation {

  def provide[From, FromG <: HList, TargetT, Modifiers <: HList]
    (from: From, targetLabel: Witness.Lt[Symbol], clz: Class[TargetT], modifiers: Modifiers)
    (implicit lg: LabelledGeneric.Aux[From, FromG],
     vp: ValueProvider[From, FromG, TargetT, targetLabel.T, Modifiers])
  : TargetT = vp.provide(lg.to(from), modifiers)

}

trait ValueProviderDerivation {

  implicit def hnilCase[From, FromG <: HList, Modifiers <: HNil, FromT, TargetT, L <: Symbol]
  (implicit fieldSelector: ops.record.Selector.Aux[FromG, L, FromT],
   fieldTransformer: DerivedTransformer[FromT, FromT, TargetT, Modifiers])
  : ValueProvider[From, FromG, TargetT, L, Modifiers] =
    (from: FromG, modifiers: Modifiers) =>
      fieldTransformer.transform(fieldSelector(from), modifiers)

  implicit def hconsFieldFunctionCase[From, FromG <: HList, T, L <: Symbol, Modifiers <: HList]
  (implicit fromLG: LabelledGeneric.Aux[From, FromG])
  : ValueProvider[From, FromG, T, L, Modifier.fieldFunction[L, From, T] :: Modifiers] =
    (from: FromG, modifiers: Modifier.fieldFunction[L, From, T] :: Modifiers) =>
      modifiers.head.f(fromLG.from(from))

  implicit def hconsRelabelCase[From, FromG <: HList, T, LFrom <: Symbol, L <: Symbol, Modifiers <: HList]
  (implicit fieldSelector: ops.record.Selector.Aux[FromG, LFrom, T])
  : ValueProvider[From, FromG, T, L, Modifier.relabel[LFrom, L] :: Modifiers] =
    (from: FromG, _: Modifier.relabel[LFrom, L] :: Modifiers) =>
      fieldSelector(from)

  implicit def hconsTailCase[From, FromG <: HList, T, L <: Symbol, M <: Modifier, Ms <: HList]
  (implicit tvp: ValueProvider[From, FromG, T, L, Ms])
  : ValueProvider[From, FromG, T, L, M :: Ms] =
    (from: FromG, modifiers: M :: Ms) =>
      tvp.provide(from, modifiers.tail)
}
