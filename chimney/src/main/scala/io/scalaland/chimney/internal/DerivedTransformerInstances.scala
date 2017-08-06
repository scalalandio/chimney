package io.scalaland.chimney.internal

import io.scalaland.chimney._
import shapeless._
import samurai._

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait BasicInstances {

  @sam implicit final def fromTransformer[T, U, Modifiers <: HList](
    implicit transformer: Transformer[T, U]
  ): DerivedTransformer[T, U, Modifiers] =
    (src: T, _: Modifiers) =>
      transformer.transform(src)

  @sam implicit final def identityTransformer[T, Modifiers <: HList]: DerivedTransformer[T, T, Modifiers] =
    (src: T, _: Modifiers) => src
}

trait ValueClassInstances {

  @sam implicit final def toValueClassTransformer[C <: AnyVal, V, Modifiers <: HList](
    implicit gen: Generic.Aux[C, V :: HNil]
  ): DerivedTransformer[V, C, Modifiers] =
    (src: V, _: Modifiers) => gen.from(src :: HNil)

  @sam implicit final def fromValueClassTransformer[C <: AnyVal, V, Modifiers <: HList](
    implicit gen: Generic.Aux[C, V :: HNil]
  ): DerivedTransformer[C, V, Modifiers] =
    (src: C, _: Modifiers) => gen.to(src).head
}

trait OptionInstances {

  @sam implicit final def optionTransformer[From, To, Modifiers <: HList](
    implicit innerTransformer: DerivedTransformer[From, To, Modifiers]
  ): DerivedTransformer[Option[From], Option[To], Modifiers] =
    (src: Option[From], modifiers: Modifiers) =>
      src.map(innerTransformer.transform(_, modifiers))

  @sam implicit final def someTransformer[From, To, Modifiers <: HList](
    implicit innerTransformer: DerivedTransformer[From, To, Modifiers]
  ): DerivedTransformer[Some[From], Option[To], Modifiers] =
    (src: Some[From], modifiers: Modifiers) =>
      Some(innerTransformer.transform(src.get, modifiers))

  @sam implicit final def noneTransformer[From, To, Modifiers <: HList](
    implicit innerTransformer: DerivedTransformer[From, To, Modifiers]
  ): DerivedTransformer[None.type, Option[To], Modifiers] =
    (_: None.type, _: Modifiers) => None
}

trait EitherInstances {

  @sam implicit final def eitherTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList](
    implicit leftTransformer: DerivedTransformer[FromL, ToL, Modifiers],
    rightTransformer: DerivedTransformer[FromR, ToR, Modifiers]
  ): DerivedTransformer[Either[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Either[FromL, FromR], modifiers: Modifiers) => src match {
      case Left(value)  => Left(leftTransformer.transform(value, modifiers))
      case Right(value) => Right(rightTransformer.transform(value, modifiers))
    }

  @sam implicit final def leftTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList](
    implicit leftTransformer: DerivedTransformer[FromL, ToL, Modifiers]
  ): DerivedTransformer[Left[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Left[FromL, FromR], modifiers: Modifiers) => {
      val Left(value) = src
      Left(leftTransformer.transform(value, modifiers))
    }

  @sam implicit final def rightTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList](
    implicit rightTransformer: DerivedTransformer[FromR, ToR, Modifiers]
  ): DerivedTransformer[Right[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Right[FromL, FromR], modifiers: Modifiers) => {
      val Right(value) = src
      Right(rightTransformer.transform(value, modifiers))
    }
}

trait CollectionInstances {

  @sam implicit final def traversableTransformer[From, To, Modifiers <: HList, M1[_], M2[_]](
    implicit innerTransformer: DerivedTransformer[From, To, Modifiers],
    ev1: M1[From] <:< Traversable[From],
    ev2: M2[To] <:< Traversable[To],
    cbf: CanBuildFrom[M1[From], To, M2[To]]
  ): DerivedTransformer[M1[From], M2[To], Modifiers] =
    (src: M1[From], modifiers: Modifiers) =>
      src.map(innerTransformer.transform(_: From, modifiers)).to[M2]

  @sam implicit final def arrayTransformer[From, To, Modifiers <: HList](
    implicit innerTransformer: DerivedTransformer[From, To, Modifiers],
    toTag: ClassTag[To]
  ): DerivedTransformer[Array[From], Array[To], Modifiers] =
    (src: Array[From], modifiers: Modifiers) =>
      src.map(innerTransformer.transform(_: From, modifiers))

  @sam implicit final def mapTransformer[FromK, ToK, FromV, ToV, Modifiers <: HList](
    implicit keyTransformer: DerivedTransformer[FromK, ToK, Modifiers],
    valueTransformer: DerivedTransformer[FromV, ToV, Modifiers]
  ): DerivedTransformer[Map[FromK, FromV], Map[ToK, ToV], Modifiers] =
    (src: Map[FromK, FromV], modifiers: Modifiers) => src.map {
      case (key, value) =>
        keyTransformer.transform(key, modifiers) -> valueTransformer.transform(value, modifiers)
    }
}

trait GenericInstances {

  @sam implicit final def genProduct[From, To, FromLG <: HList, ToLG <: HList, Modifiers <: HList](
    implicit fromLG: LabelledGeneric.Aux[From, FromLG],
    toLG: LabelledGeneric.Aux[To, ToLG],
    intermediateTransformer: Lazy[DerivedProductTransformer[From, FromLG, ToLG, Modifiers]]
  ): DerivedTransformer[From, To, Modifiers] =
    (src: From, modifiers: Modifiers) =>
      toLG.from(intermediateTransformer.value.transform(fromLG.to(src), modifiers))

  @sam implicit final def genCoproduct[From, To, FromLG <: Coproduct, ToLG <: Coproduct, Modifiers <: HList](
    implicit fromLG: LabelledGeneric.Aux[From, FromLG],
    toLG: LabelledGeneric.Aux[To, ToLG],
    intermediateTransformer: Lazy[DerivedCoproductTransformer[From, FromLG, ToLG, Modifiers]]
  ): DerivedTransformer[From, To, Modifiers] =
    (src: From, modifiers: Modifiers) =>
      toLG.from(intermediateTransformer.value.transform(fromLG.to(src), modifiers))
}
