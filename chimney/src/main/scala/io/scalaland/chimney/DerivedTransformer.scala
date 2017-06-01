package io.scalaland.chimney

import shapeless._
import shapeless.labelled._
import shapeless.tag._

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag


trait DerivedTransformer[OriginalFrom, From, To, Modifiers <: HList] {

  def transform(src: From, modifiers: Modifiers): To
}

object DerivedTransformer
  extends BasicInstances
  with ValueClassInstances
  with OptionInstances
  with EitherInstances
  with CollectionInstances
  with GenericInstances {

  def apply[From, To, Modifiers <: HList](implicit dt: DerivedTransformer[From, From, To, Modifiers]): DerivedTransformer[From, From, To, Modifiers] = dt

}

trait BasicInstances {

  implicit def fromTransformer[T, U, Modifiers <: HList]
    (implicit transformer: Transformer[T, U]): DerivedTransformer[T, T, U, Modifiers] =
    (src: T, _: Modifiers) => transformer.transform(src)

  implicit def identityTransformer[T, Modifiers <: HList]: DerivedTransformer[T, T, T, Modifiers] =
    (src: T, _: Modifiers) => src
}

trait ValueClassInstances {
  implicit def toValueClassTransformer[C <: AnyVal, V, Modifiers <: HList]
    (implicit gen: Generic.Aux[C, V :: HNil])
  : DerivedTransformer[V, V, C, Modifiers] =
    (src: V, _: Modifiers) => gen.from(src :: HNil)

  implicit def fromValueClassTransformer[C <: AnyVal, V, Modifiers <: HList]
    (implicit gen: Generic.Aux[C, V :: HNil])
  : DerivedTransformer[C, C, V, Modifiers] =
    (src: C, _: Modifiers) => gen.to(src).head
}

trait OptionInstances {

  implicit def optionTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, From, To, Modifiers])
  : DerivedTransformer[Option[From], Option[From], Option[To], Modifiers] =
    (src: Option[From], modifiers: Modifiers) => src.map(innerTransformer.transform(_, modifiers))

  implicit def someTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, From, To, Modifiers])
  : DerivedTransformer[Some[From], Some[From], Option[To], Modifiers] =
    (src: Some[From], modifiers: Modifiers) => Some(innerTransformer.transform(src.value, modifiers))

  implicit def noneTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, From, To, Modifiers])
  : DerivedTransformer[None.type, None.type, Option[To], Modifiers] =
    (_: None.type, _: Modifiers) => None
}

trait EitherInstances {

  implicit def eitherTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList]
   (implicit leftTransformer: DerivedTransformer[FromL, FromL, ToL, Modifiers],
    rightTransformer: DerivedTransformer[FromR, FromR, ToR, Modifiers])
  : DerivedTransformer[Either[FromL, FromR], Either[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Either[FromL, FromR], modifiers: Modifiers) => src match {
      case Left(value)  => Left(leftTransformer.transform(value, modifiers))
      case Right(value) => Right(rightTransformer.transform(value, modifiers))
    }

  implicit def leftTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList]
    (implicit leftTransformer: DerivedTransformer[FromL, FromL, ToL, Modifiers])
  : DerivedTransformer[Left[FromL, FromR], Left[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Left[FromL, FromR], modifiers: Modifiers) => Left(leftTransformer.transform(src.value, modifiers))

  implicit def rightTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList]
    (implicit rightTransformer: DerivedTransformer[FromR, FromR, ToR, Modifiers])
  : DerivedTransformer[Right[FromL, FromR], Right[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Right[FromL, FromR], modifiers: Modifiers) => Right(rightTransformer.transform(src.value, modifiers))
}

trait CollectionInstances {

  implicit def traversableTransformer[From, To, Modifiers <: HList, M1[_], M2[_]]
    (implicit innerTransformer: DerivedTransformer[From, From, To, Modifiers],
     ev1: M1[From] <:< Traversable[From],
     ev2: M2[To] <:< Traversable[To],
     cbf: CanBuildFrom[M1[From], To, M2[To]])
  : DerivedTransformer[M1[From], M1[From], M2[To], Modifiers] =
    (src: M1[From], modifiers: Modifiers) =>
      src.map(innerTransformer.transform(_: From, modifiers)).to[M2]

  implicit def arrayTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, From, To, Modifiers], toTag: ClassTag[To])
  : DerivedTransformer[Array[From], Array[From], Array[To], Modifiers] =
    (src: Array[From], modifiers: Modifiers) => src.map(innerTransformer.transform(_: From, modifiers))

  implicit def mapTransformer[FromK, ToK, FromV, ToV, Modifiers <: HList]
    (implicit keyTransformer: DerivedTransformer[FromK, FromK, ToK, Modifiers],
     valueTransformer: DerivedTransformer[FromV, FromV, ToV, Modifiers])
  : DerivedTransformer[Map[FromK, FromV], Map[FromK, FromV], Map[ToK, ToV], Modifiers] =
    (src: Map[FromK, FromV], modifiers: Modifiers) => src.map { case (k, v) =>
      keyTransformer.transform(k, modifiers) -> valueTransformer.transform(v, modifiers)
    }

}

trait GenericInstances extends LowPriorityGenericInstances {

  implicit def hnilCase[From, FromG <: HList, Modifiers <: HList]
  : DerivedTransformer[From, FromG, HNil, Modifiers] =
    (_: FromG, _: Modifiers) => HNil

  implicit def hconsCase[From, FromG <: HList, Label <: Symbol, ToFieldT, TailToG <: HList, Modifiers <: HList]
  (implicit vp: ValueProvider[From, FromG, ToFieldT, Label, Modifiers],
   tailTransformer: DerivedTransformer[From, FromG, TailToG, Modifiers])
  : DerivedTransformer[From, FromG, FieldType[Label, ToFieldT] :: TailToG, Modifiers] = {
    (src: FromG, modifiers: Modifiers) =>
      field[Label](vp.provide(src, modifiers)) :: tailTransformer.transform(src, modifiers)
  }

  // $COVERAGE-OFF$
  implicit def cnilCase[From, ToG <: Coproduct, Modifiers <: HList]
  : DerivedTransformer[From, CNil, ToG, Modifiers] =
    (_: CNil, _: Modifiers) => null.asInstanceOf[ToG]
  // $COVERAGE-ON$

  implicit def coproductCase[From, TailFromG <: Coproduct, Label <: Symbol, HeadT, ToG <: Coproduct, Modifiers <: HList]
  (implicit cip: CoproductInstanceProvider[ToG, Label, HeadT, Modifiers],
   tailTransformer: DerivedTransformer[From, TailFromG, ToG, Modifiers])
  : DerivedTransformer[From, FieldType[Label, HeadT] :+: TailFromG, ToG, Modifiers] =
    (src: FieldType[Label, HeadT] :+: TailFromG, modifiers: Modifiers) =>
      (src: FieldType[Label, HeadT] :+: TailFromG) match {
        case Inl(hd) => cip.provide(hd, modifiers)
        case Inr(tl) => tailTransformer.transform(tl, modifiers)
      }

  implicit def gen[From, To, FromG, ToG, Modifiers <: HList]
  (implicit fromLG: LabelledGeneric.Aux[From, FromG],
   toLG: LabelledGeneric.Aux[To, ToG],
   genTransformer: Lazy[DerivedTransformer[From, FromG, ToG, Modifiers]])
  : DerivedTransformer[From, From, To, Modifiers] = {
    (src: From, modifiers: Modifiers) =>
      toLG.from(genTransformer.value.transform(fromLG.to(src), modifiers))
  }
}

trait LowPriorityGenericInstances {

  implicit def gen2[From, To, FromG, ToG, Modifiers <: HList]
  (implicit fromLG: LabelledGeneric.Aux[From, FromG],
   toLG: LabelledGeneric.Aux[To, ToG],
   genTransformer: DerivedTransformer[From, FromG, ToG, Modifiers])
  : DerivedTransformer[From, From, To, Modifiers] = {
    (src: From, modifiers: Modifiers) =>
      toLG.from(genTransformer.transform(fromLG.to(src), modifiers))
  }
}
