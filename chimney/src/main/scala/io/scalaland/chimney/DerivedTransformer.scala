package io.scalaland.chimney

import shapeless._
import shapeless.labelled._
import shapeless.tag._


trait DerivedTransformer[From, To, Modifiers <: HList] {

  def transform(src: From, modifiers: Modifiers): To
}

object DerivedTransformer {

  implicit def fromNonDerived[T, U, Modifiers <: HList]
    (implicit transformer: Transformer[T, U]): DerivedTransformer[T, U, Modifiers] =
    (src: T, _: Modifiers) =>
      transformer.transform(src)

  implicit def identityTransformer[T, Modifiers <: HList]: DerivedTransformer[T, T, Modifiers] =
    (src: T, _: Modifiers) => src

  implicit def toValueClassTransformer[C <: AnyVal, V, Modifiers <: HList]
    (implicit gen: Generic.Aux[C, V :: HNil])
  : DerivedTransformer[V, C, Modifiers] =
    (src: V, _: Modifiers) => gen.from(src :: HNil)

  implicit def fromValueClassTransformer[C <: AnyVal, V, Modifiers <: HList]
    (implicit gen: Generic.Aux[C, V :: HNil])
  : DerivedTransformer[C, V, Modifiers] =
    (src: C, _: Modifiers) => gen.to(src).head

  implicit def hnilCase[From, FromG <: HList, Modifiers <: HList]
  : DerivedTransformer[FromG @@ From, HNil, Modifiers] =
    (_: FromG @@ From, _: Modifiers) => HNil

  implicit def hconsCase[From, FromG <: HList, Label <: Symbol, ToFieldT, TailToG <: HList, Modifiers <: HList]
    (implicit vp: ValueProvider[FromG, From, ToFieldT, Label, Modifiers],
     tailTransformer: DerivedTransformer[FromG @@ From, TailToG, Modifiers])
  : DerivedTransformer[FromG @@ From, FieldType[Label, ToFieldT] :: TailToG, Modifiers] = {
    (src: FromG @@ From, modifiers: Modifiers) =>
      field[Label](vp.provide(src, modifiers)) :: tailTransformer.transform(src, modifiers)
  }

  implicit def gen[From, To, FromG, ToG, Modifiers <: HList]
    (implicit fromLG: LabelledGeneric.Aux[From, FromG],
     toLG: LabelledGeneric.Aux[To, ToG],
     genTransformer: DerivedTransformer[FromG @@ From, ToG, Modifiers]): DerivedTransformer[From, To, Modifiers] = {
    (src: From, modifiers: Modifiers) =>
      toLG.from(genTransformer.transform(tag[From](fromLG.to(src)), modifiers))
  }
}
