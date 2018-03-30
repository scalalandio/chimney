package io.scalaland.chimney.internal

import io.scalaland.chimney._
import shapeless._

trait ValueProvider[From, FromLG, TargetT, Label <: Symbol, Modifiers <: HList, RecursiveModifiers <: HList] {

  def provide(src: FromLG, modifiers: Modifiers, recursiveModifiers: RecursiveModifiers): TargetT
}

object ValueProvider extends ValueProviderDerivation {

  final def provide[From, FromLG <: HList, TargetT, Modifiers <: HList](from: From,
                                                                        targetLabel: Witness.Lt[Symbol],
                                                                        clz: Class[TargetT],
                                                                        modifiers: Modifiers)(
    implicit lg: LabelledGeneric.Aux[From, FromLG],
    vp: ValueProvider[From, FromLG, TargetT, targetLabel.T, Modifiers, HNil]
  ): TargetT = vp.provide(lg.to(from), modifiers, HNil)

  final def instance[From, FromLG, TargetT, Label <: Symbol, Modifiers <: HList, RecursiveModifiers <: HList](
    f: (FromLG, Modifiers, RecursiveModifiers) => TargetT
  ): ValueProvider[From, FromLG, TargetT, Label, Modifiers, RecursiveModifiers] =
    new ValueProvider[From, FromLG, TargetT, Label, Modifiers, RecursiveModifiers] {
      @inline final def provide(src: FromLG, modifiers: Modifiers, recursiveModifiers: RecursiveModifiers): TargetT =
        f(src, modifiers, recursiveModifiers)
    }
}

trait ValueProviderDerivation extends LowPriorityValueProviderDerivation {

  implicit final def hnilCase[From,
                              FromLG <: HList,
                              TargetT,
                              Label <: Symbol,
                              Modifiers <: HNil,
                              RecursiveModifiers <: HList,
                              FromT](
    implicit fieldSelector: ops.record.Selector.Aux[FromLG, Label, FromT],
    fieldTransformer: DerivedTransformer[FromT, TargetT, RecursiveModifiers]
  ): ValueProvider[From, FromLG, TargetT, Label, Modifiers, RecursiveModifiers] =
    ValueProvider.instance { (src: FromLG, _: Modifiers, recursiveModifiers: RecursiveModifiers) =>
      fieldTransformer.transform(fieldSelector(src), recursiveModifiers)
    }

  implicit final def hconsFieldFunctionCase[From,
                                            FromLG <: HList,
                                            TargetT,
                                            ModT,
                                            Label <: Symbol,
                                            Modifiers <: HList,
                                            RecursiveModifiers <: HList](
    implicit fromLG: LabelledGeneric.Aux[From, FromLG],
    lubTargetTModT: Lub[TargetT, ModT, TargetT]
  ): ValueProvider[From, FromLG, TargetT, Label, Modifier.fieldFunction[Label, From, ModT] :: Modifiers, RecursiveModifiers] =
    ValueProvider.instance {
      (src: FromLG, modifiers: Modifier.fieldFunction[Label, From, ModT] :: Modifiers, _: RecursiveModifiers) =>
        lubTargetTModT.right(modifiers.head.map(fromLG.from(src)))
    }

  implicit final def hconsRelabelCase[From,
                                      FromLG <: HList,
                                      TargetT,
                                      LabelFrom <: Symbol,
                                      LabelTo <: Symbol,
                                      Modifiers <: HList,
                                      RecursiveModifiers <: HList](
    implicit fieldSelector: ops.record.Selector.Aux[FromLG, LabelFrom, TargetT]
  ): ValueProvider[From, FromLG, TargetT, LabelTo, Modifier.relabel[LabelFrom, LabelTo] :: Modifiers, RecursiveModifiers] =
    ValueProvider.instance {
      (src: FromLG, _: Modifier.relabel[LabelFrom, LabelTo] :: Modifiers, _: RecursiveModifiers) =>
        fieldSelector(src)
    }

  implicit final def hconsDisableDefaultValuesCase[From,
                                                   FromLG <: HList,
                                                   TargetT,
                                                   Label <: Symbol,
                                                   Ms <: HList,
                                                   RecursiveModifiers <: HList](
    implicit vp: ValueProvider[From, FromLG, TargetT, Label, Ms, Modifier.disableDefaultValues :: RecursiveModifiers]
  ): ValueProvider[From, FromLG, TargetT, Label, Modifier.disableDefaultValues :: Ms, RecursiveModifiers] =
    ValueProvider.instance {
      (src: FromLG, modifiers: Modifier.disableDefaultValues :: Ms, recursiveModifiers: RecursiveModifiers) =>
        vp.provide(src, modifiers.tail, new Modifier.disableDefaultValues :: recursiveModifiers)
    }

}

trait LowPriorityValueProviderDerivation {

  implicit final def hconsTailCase[From,
                                   FromLG <: HList,
                                   TargetT,
                                   Label <: Symbol,
                                   M <: Modifier,
                                   Ms <: HList,
                                   RecursiveModifiers <: HList](
    implicit vp: ValueProvider[From, FromLG, TargetT, Label, Ms, RecursiveModifiers]
  ): ValueProvider[From, FromLG, TargetT, Label, M :: Ms, RecursiveModifiers] =
    ValueProvider.instance { (src: FromLG, modifiers: M :: Ms, recursiveModifiers: RecursiveModifiers) =>
      vp.provide(src, modifiers.tail, recursiveModifiers)
    }
}
