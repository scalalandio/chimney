package io.scalaland.chimney

import shapeless._
import shapeless.labelled._
import shapeless.tag._

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag


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

  implicit def optionTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, To, Modifiers])
  : DerivedTransformer[Option[From], Option[To], Modifiers] =
    (src: Option[From], modifiers: Modifiers) => src.map(innerTransformer.transform(_, modifiers))

  implicit def someTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, To, Modifiers])
  : DerivedTransformer[Some[From], Option[To], Modifiers] =
    (src: Some[From], modifiers: Modifiers) => Some(innerTransformer.transform(src.value, modifiers))

  implicit def leftTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList]
  (implicit leftTransformer: DerivedTransformer[FromL, ToL, Modifiers])
  : DerivedTransformer[Left[FromL, FromR], Left[ToL, ToR], Modifiers] =
    (src: Left[FromL, FromR], modifiers: Modifiers) => Left(leftTransformer.transform(src.value, modifiers))

  implicit def rightTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList]
  (implicit rightTransformer: DerivedTransformer[FromR, ToR, Modifiers])
  : DerivedTransformer[Right[FromL, FromR], Right[ToL, ToR], Modifiers] =
    (src: Right[FromL, FromR], modifiers: Modifiers) => Right(rightTransformer.transform(src.value, modifiers))

  implicit def eitherTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList]
    (implicit leftTransformer: DerivedTransformer[FromL, ToL, Modifiers],
     rightTransformer: DerivedTransformer[FromR, ToR, Modifiers])
  : DerivedTransformer[Either[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Either[FromL, FromR], modifiers: Modifiers) => src match {
      case Left(value)  => Left(leftTransformer.transform(value, modifiers))
      case Right(value) => Right(rightTransformer.transform(value, modifiers))
    }

  implicit def traversableTransformer[From, To, Modifiers <: HList, M[_]]
    (implicit innerTransformer: DerivedTransformer[From, To, Modifiers],
     ev1: M[From] <:< Traversable[From],
     ev2: M[To] <:< Traversable[To],
     cbf: CanBuildFrom[M[From], To, M[To]])
  : DerivedTransformer[M[From], M[To], Modifiers] =
    (src: M[From], modifiers: Modifiers) => src.map(innerTransformer.transform(_: From, modifiers)).to[M]

  implicit def arrayTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, To, Modifiers], toTag: ClassTag[To])
  : DerivedTransformer[Array[From], Array[To], Modifiers] =
    (src: Array[From], modifiers: Modifiers) => src.map(innerTransformer.transform(_: From, modifiers))
}
