package io.scalaland.chimney.internal

import io.scalaland.chimney._
import shapeless._

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait DerivedTransformerInstances extends ToValueClassInstance {

  implicit final def identityTransformer[T, Modifiers <: HList]: DerivedTransformer[T, T, Modifiers] =
    (src: T, _: Modifiers) => src
}

trait ToValueClassInstance extends FromValueClassInstance {

  implicit final def toValueClassTransformer[C, V, Modifiers <: HList](
    implicit ev: C <:< AnyVal,
    gen: Generic.Aux[C, V :: HNil]
  ): DerivedTransformer[V, C, Modifiers] =
    (src: V, _: Modifiers) => gen.from(src :: HNil)
}

trait FromValueClassInstance extends TraversableInstance {

  implicit final def fromValueClassTransformer[C, V, Modifiers <: HList](
    implicit ev: C <:< AnyVal,
    gen: Generic.Aux[C, V :: HNil]
  ): DerivedTransformer[C, V, Modifiers] =
    (src: C, _: Modifiers) => gen.to(src).head
}

trait TraversableInstance extends OptionInstance {

  implicit final def traversableTransformer[From, To, Modifiers <: HList, M1[X] <: Traversable[X], M2[Y] <: Traversable[
    Y
  ]](implicit innerTransformer: DerivedTransformer[From, To, Modifiers],
     cbf: CanBuildFrom[M1[From], To, M2[To]]): DerivedTransformer[M1[From], M2[To], Modifiers] =
    (src: M1[From], modifiers: Modifiers) => src.map(innerTransformer.transform(_: From, modifiers)).to[M2]
}

trait OptionInstance extends GenericProductInstance {

  implicit final def optionTransformer[From, To, Modifiers <: HList](
    implicit innerTransformer: DerivedTransformer[From, To, Modifiers]
  ): DerivedTransformer[Option[From], Option[To], Modifiers] =
    (src: Option[From], modifiers: Modifiers) => src.map(innerTransformer.transform(_, modifiers))
}

trait GenericProductInstance extends GenericCoproductInstance {

  implicit final def genProduct[From, To, FromLG <: HList, ToLG <: HList, Modifiers <: HList](
    implicit fromLG: LabelledGeneric.Aux[From, FromLG],
    toLG: LabelledGeneric.Aux[To, ToLG],
    intermediateTransformer: Lazy[DerivedProductTransformer[From, FromLG, To, ToLG, Modifiers]]
  ): DerivedTransformer[From, To, Modifiers] =
    (src: From, modifiers: Modifiers) => toLG.from(intermediateTransformer.value.transform(fromLG.to(src), modifiers))
}

trait GenericCoproductInstance extends MapInstance {

  implicit final def genCoproduct[From, To, FromLG <: Coproduct, ToLG <: Coproduct, Modifiers <: HList](
    implicit fromLG: LabelledGeneric.Aux[From, FromLG],
    toLG: LabelledGeneric.Aux[To, ToLG],
    intermediateTransformer: Lazy[DerivedCoproductTransformer[From, FromLG, ToLG, Modifiers]]
  ): DerivedTransformer[From, To, Modifiers] =
    (src: From, modifiers: Modifiers) => toLG.from(intermediateTransformer.value.transform(fromLG.to(src), modifiers))
}

trait MapInstance extends ArrayInstance {

  implicit final def mapTransformer[FromK, ToK, FromV, ToV, Modifiers <: HList](
    implicit keyTransformer: DerivedTransformer[FromK, ToK, Modifiers],
    valueTransformer: DerivedTransformer[FromV, ToV, Modifiers]
  ): DerivedTransformer[Map[FromK, FromV], Map[ToK, ToV], Modifiers] =
    (src: Map[FromK, FromV], modifiers: Modifiers) =>
      src.map {
        case (key, value) =>
          keyTransformer.transform(key, modifiers) -> valueTransformer
            .transform(value, modifiers)
    }
}

trait ArrayInstance extends LeftInstance {

  implicit final def arrayTransformer[From, To, Modifiers <: HList](
    implicit
    innerTransformer: DerivedTransformer[From, To, Modifiers],
    toTag: ClassTag[To]
  ): DerivedTransformer[Array[From], Array[To], Modifiers] =
    (src: Array[From], modifiers: Modifiers) => src.map(innerTransformer.transform(_: From, modifiers))
}

trait LeftInstance extends RightInstance {

  implicit final def leftTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList](
    implicit leftTransformer: DerivedTransformer[FromL, ToL, Modifiers]
  ): DerivedTransformer[Left[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Left[FromL, FromR], modifiers: Modifiers) => {
      val Left(value) = src
      Left(leftTransformer.transform(value, modifiers))
    }
}

trait RightInstance {

  implicit final def rightTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList](
    implicit rightTransformer: DerivedTransformer[FromR, ToR, Modifiers]
  ): DerivedTransformer[Right[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Right[FromL, FromR], modifiers: Modifiers) => {
      val Right(value) = src
      Right(rightTransformer.transform(value, modifiers))
    }
}
