package io.scalaland.chimney.internal

import io.scalaland.chimney._
import shapeless._

trait ValueProvider[From, FromLG, TargetT, Label <: Symbol, Modifiers <: HList] {

  def provide(src: FromLG, modifiers: Modifiers): TargetT
}

object ValueProvider extends ValueProviderDerivation {

  final def provide[From, FromLG <: HList, TargetT, Modifiers <: HList](from: From,
                                                                        targetLabel: Witness.Lt[Symbol],
                                                                        clz: Class[TargetT],
                                                                        modifiers: Modifiers)(
    implicit lg: LabelledGeneric.Aux[From, FromLG],
    vp: ValueProvider[From, FromLG, TargetT, targetLabel.T, Modifiers]
  ): TargetT = vp.provide(lg.to(from), modifiers)

  final def instance[From, FromLG, TargetT, Label <: Symbol, Modifiers <: HList](
    f: (FromLG, Modifiers) => TargetT
  ): ValueProvider[From, FromLG, TargetT, Label, Modifiers] =
    new ValueProvider[From, FromLG, TargetT, Label, Modifiers] {
      @inline final def provide(src: FromLG, modifiers: Modifiers): TargetT = f(src, modifiers)
    }
}

trait ValueProviderDerivation {

  implicit final def hnilCase[From, FromLG <: HList, TargetT, Label <: Symbol, Modifiers <: HNil, FromT](
    implicit fieldSelector: ops.record.Selector.Aux[FromLG, Label, FromT],
    fieldTransformer: DerivedTransformer[FromT, TargetT, Modifiers]
  ): ValueProvider[From, FromLG, TargetT, Label, Modifiers] =
    ValueProvider.instance { (src: FromLG, modifiers: Modifiers) =>
      fieldTransformer.transform(fieldSelector(src), modifiers)
    }

  implicit final def hconsFieldFunctionCase[From, FromLG <: HList, TargetT, ModT, Label <: Symbol, Modifiers <: HList](
    implicit fromLG: LabelledGeneric.Aux[From, FromLG],
    lubTargetTModT: Lub[TargetT, ModT, TargetT]
  ): ValueProvider[From, FromLG, TargetT, Label, Modifier.fieldFunction[Label, From, ModT] :: Modifiers] =
    ValueProvider.instance { (src: FromLG, modifiers: Modifier.fieldFunction[Label, From, ModT] :: Modifiers) =>
      lubTargetTModT.right(modifiers.head.map(fromLG.from(src)))
    }

  implicit final def hconsRelabelCase[From,
                                      FromLG <: HList,
                                      TargetT,
                                      LabelFrom <: Symbol,
                                      LabelTo <: Symbol,
                                      Modifiers <: HList](
    implicit fieldSelector: ops.record.Selector.Aux[FromLG, LabelFrom, TargetT]
  ): ValueProvider[From, FromLG, TargetT, LabelTo, Modifier.relabel[LabelFrom, LabelTo] :: Modifiers] =
    ValueProvider.instance { (src: FromLG, _: Modifier.relabel[LabelFrom, LabelTo] :: Modifiers) =>
      fieldSelector(src)
    }

  implicit final def hconsTailCase[From, FromLG <: HList, TargetT, Label <: Symbol, M <: Modifier, Ms <: HList](
    implicit vp: ValueProvider[From, FromLG, TargetT, Label, Ms]
  ): ValueProvider[From, FromLG, TargetT, Label, M :: Ms] =
    ValueProvider.instance { (src: FromLG, modifiers: M :: Ms) =>
      vp.provide(src, modifiers.tail)
    }
}
