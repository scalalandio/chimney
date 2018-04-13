package io.scalaland.chimney.internal

import io.scalaland.chimney._
import shapeless._

trait ValueProvider[From, FromLG, TargetT, Label <: Symbol, Modifiers <: HList] {

  def provide(src: FromLG, modifiers: Modifiers): TargetT
}

object ValueProvider extends ValueProviderDerivation {

  final def instance[From, FromLG, TargetT, Label <: Symbol, Modifiers <: HList](
    f: (FromLG, Modifiers) => TargetT
  ): ValueProvider[From, FromLG, TargetT, Label, Modifiers] =
    new ValueProvider[From, FromLG, TargetT, Label, Modifiers] {
      @inline final def provide(src: FromLG, modifiers: Modifiers): TargetT =
        f(src, modifiers)
    }
}

trait ValueProviderDerivation extends ValueProviderHNilInstance {

  implicit final def hnilDefaultValuesRecCase[From, FromLG <: HList, TargetT, Label <: Symbol, FromT](
    implicit fieldSelector: ops.record.Selector.Aux[FromLG, Label, FromT],
    fieldTransformer: DerivedTransformer[FromT, TargetT, Modifier.enableDefaultValues :: HNil]
  ): ValueProvider[From, FromLG, TargetT, Label, Modifier.enableDefaultValues :: HNil] =
    ValueProvider.instance { (src: FromLG, modifiers: Modifier.enableDefaultValues :: HNil) =>
      fieldTransformer.transform(fieldSelector(src), modifiers)
    }
}

trait ValueProviderHNilInstance extends ValueProviderFieldFunctionInstance {

  implicit final def hnilCase[From, FromLG <: HList, TargetT, Label <: Symbol, FromT](
    implicit fieldSelector: ops.record.Selector.Aux[FromLG, Label, FromT],
    fieldTransformer: DerivedTransformer[FromT, TargetT, HNil]
  ): ValueProvider[From, FromLG, TargetT, Label, HNil] =
    ValueProvider.instance { (src: FromLG, modifiers: HNil) =>
      fieldTransformer.transform(fieldSelector(src), modifiers)
    }
}

trait ValueProviderFieldFunctionInstance extends ValueProviderRelabelInstance {

  implicit final def hconsFieldFunctionCase[From, FromLG <: HList, TargetT, ModT, Label <: Symbol, Modifiers <: HList](
    implicit fromLG: LabelledGeneric.Aux[From, FromLG],
    lubTargetTModT: Lub[TargetT, ModT, TargetT]
  ): ValueProvider[From, FromLG, TargetT, Label, Modifier.fieldFunction[Label, From, ModT] :: Modifiers] =
    ValueProvider.instance { (src: FromLG, modifiers: Modifier.fieldFunction[Label, From, ModT] :: Modifiers) =>
      lubTargetTModT.right(modifiers.head.map(fromLG.from(src)))
    }
}

trait ValueProviderRelabelInstance extends ValueProviderHConsTailInstance {

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
}

trait ValueProviderHConsTailInstance {

  implicit final def hconsTailCase[From, FromLG <: HList, TargetT, Label <: Symbol, M <: Modifier, Ms <: HList](
    implicit vp: ValueProvider[From, FromLG, TargetT, Label, Ms]
  ): ValueProvider[From, FromLG, TargetT, Label, M :: Ms] =
    ValueProvider.instance { (src: FromLG, modifiers: M :: Ms) =>
      vp.provide(src, modifiers.tail)
    }
}
