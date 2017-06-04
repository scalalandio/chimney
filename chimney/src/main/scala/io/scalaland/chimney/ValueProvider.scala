package io.scalaland.chimney

import shapeless.{::, HList, HNil, LabelledGeneric, Witness, ops}

/** Provides a value of a Product type.
  *
  * @tparam From original type
  * @tparam FromLG generic representation of [[From]]
  * @tparam TargetT target field's type
  * @tparam Label field name used in original Product type
  * @tparam Modifiers list of modifiers that will be traversed before any attempt to obtain values the default way
  */
trait ValueProvider[From, FromLG, TargetT, Label <: Symbol, Modifiers <: HList] {

  /** Extracts target value from generic representation of original value.
    *
    * @param src generic representation of original value
    * @param modifiers list of modifiers matching [[Modifiers]] type
    * @return extracted target field's value
    */
  def provide(src: FromLG, modifiers: Modifiers): TargetT
}

object ValueProvider extends ValueProviderDerivation {

  final def provide[From, FromLG <: HList, TargetT, Modifiers <: HList](
    from: From,
    targetLabel: Witness.Lt[Symbol],
    clz: Class[TargetT],
    modifiers: Modifiers
  )(
    implicit
    lg: LabelledGeneric.Aux[From, FromLG],
    vp: ValueProvider[From, FromLG, TargetT, targetLabel.T, Modifiers]
  ): TargetT = vp.provide(lg.to(from), modifiers)
}

trait ValueProviderDerivation {

  implicit final def hnilCase[From, FromLG <: HList, TargetT, Label <: Symbol, Modifiers <: HNil, FromT](
    implicit
    fieldSelector: ops.record.Selector.Aux[FromLG, Label, FromT],
    fieldTransformer: DerivedTransformer[FromT, TargetT, Modifiers]
  ): ValueProvider[From, FromLG, TargetT, Label, Modifiers] =
    (src: FromLG, modifiers: Modifiers) => fieldTransformer.transform(fieldSelector(src), modifiers)

  implicit final def hconsFieldFunctionCase[From, FromLG <: HList, TargetT, Label <: Symbol, Modifiers <: HList](
    implicit
    fromLG: LabelledGeneric.Aux[From, FromLG]
  ): ValueProvider[From, FromLG, TargetT, Label, Modifier.fieldFunction[Label, From, TargetT] :: Modifiers] =
    (src: FromLG, modifiers: Modifier.fieldFunction[Label, From, TargetT] :: Modifiers) =>
      modifiers.head.map(fromLG.from(src))

  implicit final def hconsRelabelCase[From, FromLG <: HList, TargetT, LabelFrom <: Symbol, LabelTo <: Symbol, Modifiers <: HList](
    implicit
    fieldSelector: ops.record.Selector.Aux[FromLG, LabelFrom, TargetT]
  ): ValueProvider[From, FromLG, TargetT, LabelTo, Modifier.relabel[LabelFrom, LabelTo] :: Modifiers] =
    (src: FromLG, _: Modifier.relabel[LabelFrom, LabelTo] :: Modifiers) => fieldSelector(src)

  implicit final def hconsTailCase[From, FromLG <: HList, TargetT, Label <: Symbol, M <: Modifier, Ms <: HList](
    implicit
    vp: ValueProvider[From, FromLG, TargetT, Label, Ms]
  ): ValueProvider[From, FromLG, TargetT, Label, M :: Ms] =
    (src: FromLG, modifiers: M :: Ms) => vp.provide(src, modifiers.tail)
}
