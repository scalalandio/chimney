package io.scalaland.chimney

import shapeless.{::, =:!=, HList, HNil, LabelledGeneric, Lazy, Witness, ops}


trait ValueProvider[FromG <: HList, From, TargetLabel <: Symbol, Ms <: HList] {
  type TargetT
  def provide(from: FromG, modifiers: Ms): TargetT
}

object ValueProvider extends LowPriValueProvider {

  type Aux[FromG_ <: HList, From_, TargetLabel_ <: Symbol, Ms_ <: HList, TargetT_] =
    ValueProvider[FromG_, From_, TargetLabel_, Ms_] { type TargetT = TargetT_ }

  private[chimney] def instance[FromG <: HList, From, TargetLabel <: Symbol, Ms <: HList, TargetT_]
    (f: (FromG, Ms) => TargetT_): ValueProvider.Aux[FromG, From, TargetLabel, Ms, TargetT_] =
    new ValueProvider[FromG, From, TargetLabel, Ms] {
      type TargetT = TargetT_
      def provide(from: FromG, modifiers: Ms): TargetT_ = f(from, modifiers)
    }

  def provide[From, FromG <: HList, TargetT, Modifiers <: HList]
    (from: From, targetLabel: Witness.Lt[Symbol], clz: Class[TargetT], modifiers: Modifiers)
    (implicit lg: LabelledGeneric.Aux[From, FromG],
     vp: ValueProvider.Aux[FromG, From, targetLabel.T, Modifiers, TargetT])
  : TargetT = vp.provide(lg.to(from), modifiers)


  implicit def hnilCase[FromG <: HList, From, SourceT, TargetT, TargetLabel <: Symbol, Ms <: HNil]
    (implicit fieldSelector: ops.record.Selector.Aux[FromG, TargetLabel, SourceT],
     fieldTransformer: Lazy[DerivedTransformer[SourceT, TargetT, Ms]])
  : ValueProvider.Aux[FromG, From, TargetLabel, Ms, TargetT] =
    instance {
      (from: FromG, modifiers: Ms) =>
        fieldTransformer.value.transform(fieldSelector(from), modifiers)
    }

  implicit def hconsFieldFunctionCase[FromG <: HList, From, TargetT, TargetLabel <: Symbol, MLabel <: Symbol, Ms <: HNil]
    (implicit fromLG: LabelledGeneric.Aux[From, FromG],
     eq: MLabel =:= TargetLabel)
  : ValueProvider.Aux[FromG, From, TargetLabel, FieldFunctionModifier[MLabel, From, TargetT] :: Ms, TargetT] =
    instance {
      (from: FromG, modifiers: FieldFunctionModifier[MLabel, From, TargetT] :: Ms) =>
        modifiers.head.f(fromLG.from(from))
    }

  implicit def hconsRelabelCase[FromG <: HList, From, TargetT, TargetLabel <: Symbol, MFromLabel <: Symbol, MToLabel <: Symbol, Ms <: HNil]
    (implicit fieldSelector: ops.record.Selector.Aux[FromG, MFromLabel, TargetT],
     eq: MToLabel =:= TargetLabel)
  : ValueProvider.Aux[FromG, From, TargetLabel, RelabelModifier[MFromLabel, MToLabel] :: Ms, TargetT] =
    instance {
      (from: FromG, _: RelabelModifier[MFromLabel, MToLabel] :: Ms) =>
        fieldSelector(from)
    }


}

trait LowPriValueProvider {


  implicit def hconsCase[FromG <: HList, From, TargetLabel <: Symbol, M, Ms <: HList, TargetT]
  (implicit tvp: ValueProvider.Aux[FromG, From, TargetLabel, Ms, TargetT])
  // does not compile if this constraint is enabled
  //   ev1: M =:!= FieldFunctionModifier[TargetLabel, From, TargetT])
  : ValueProvider.Aux[FromG, From, TargetLabel, M :: Ms, TargetT] =
    ValueProvider.instance {
      (from: FromG, modifiers: M :: Ms) =>
        tvp.provide(from, modifiers.tail)
    }
}
