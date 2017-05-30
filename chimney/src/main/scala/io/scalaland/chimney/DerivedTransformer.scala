package io.scalaland.chimney

import shapeless._
import shapeless.labelled._
import shapeless.tag._

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag


trait DerivedTransformer[From, To, Modifiers <: HList] {

  def transform(src: From, modifiers: Modifiers): To
}

object DerivedTransformer
  extends BasicInstances
  with ValueClassInstances
  with OptionInstances
  with EitherInstances
  with CollectionInstances
  with GenericInstances

trait BasicInstances {

  implicit def fromTransformer[T, U, Modifiers <: HList]
    (implicit transformer: Transformer[T, U]): DerivedTransformer[T, U, Modifiers] =
    (src: T, _: Modifiers) =>
      transformer.transform(src)

  implicit def identityTransformer[T, Modifiers <: HList]: DerivedTransformer[T, T, Modifiers] =
    (src: T, _: Modifiers) => src
}

trait ValueClassInstances {
  implicit def toValueClassTransformer[C <: AnyVal, V, Modifiers <: HList]
    (implicit gen: Generic.Aux[C, V :: HNil])
  : DerivedTransformer[V, C, Modifiers] =
    (src: V, _: Modifiers) => gen.from(src :: HNil)

  implicit def fromValueClassTransformer[C <: AnyVal, V, Modifiers <: HList]
    (implicit gen: Generic.Aux[C, V :: HNil])
  : DerivedTransformer[C, V, Modifiers] =
    (src: C, _: Modifiers) => gen.to(src).head
}

trait OptionInstances {

  implicit def optionTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, To, Modifiers])
  : DerivedTransformer[Option[From], Option[To], Modifiers] =
    (src: Option[From], modifiers: Modifiers) => src.map(innerTransformer.transform(_, modifiers))

  implicit def someTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, To, Modifiers])
  : DerivedTransformer[Some[From], Option[To], Modifiers] =
    (src: Some[From], modifiers: Modifiers) => Some(innerTransformer.transform(src.value, modifiers))

  implicit def noneTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, To, Modifiers])
  : DerivedTransformer[None.type, Option[To], Modifiers] =
    (_: None.type, _: Modifiers) => None
}

trait EitherInstances {

  implicit def eitherTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList]
   (implicit leftTransformer: DerivedTransformer[FromL, ToL, Modifiers],
    rightTransformer: DerivedTransformer[FromR, ToR, Modifiers])
  : DerivedTransformer[Either[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Either[FromL, FromR], modifiers: Modifiers) => src match {
      case Left(value)  => Left(leftTransformer.transform(value, modifiers))
      case Right(value) => Right(rightTransformer.transform(value, modifiers))
    }

  implicit def leftTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList]
    (implicit leftTransformer: DerivedTransformer[FromL, ToL, Modifiers])
  : DerivedTransformer[Left[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Left[FromL, FromR], modifiers: Modifiers) => Left(leftTransformer.transform(src.value, modifiers))

  implicit def rightTransformer[FromL, ToL, FromR, ToR, Modifiers <: HList]
    (implicit rightTransformer: DerivedTransformer[FromR, ToR, Modifiers])
  : DerivedTransformer[Right[FromL, FromR], Either[ToL, ToR], Modifiers] =
    (src: Right[FromL, FromR], modifiers: Modifiers) => Right(rightTransformer.transform(src.value, modifiers))
}

trait CollectionInstances {

  implicit def traversableTransformer[From, To, Modifiers <: HList, M[_]]
    (implicit innerTransformer: DerivedTransformer[From, To, Modifiers],
     ev1: M[From] <:< Traversable[From],
     ev2: M[To] <:< Traversable[To],
     cbf: CanBuildFrom[M[From], To, M[To]])
  : DerivedTransformer[M[From], M[To], Modifiers] =
    (src: M[From], modifiers: Modifiers) =>
      src.map(innerTransformer.transform(_: From, modifiers)).to[M]

  implicit def arrayTransformer[From, To, Modifiers <: HList]
    (implicit innerTransformer: DerivedTransformer[From, To, Modifiers], toTag: ClassTag[To])
  : DerivedTransformer[Array[From], Array[To], Modifiers] =
    (src: Array[From], modifiers: Modifiers) => src.map(innerTransformer.transform(_: From, modifiers))

  implicit def mapTransformer[FromK, ToK, FromV, ToV, Modifiers <: HList]
    (implicit keyTransformer: DerivedTransformer[FromK, ToK, Modifiers],
     valueTransformer: DerivedTransformer[FromV, ToV, Modifiers])
  : DerivedTransformer[Map[FromK, FromV], Map[ToK, ToV], Modifiers] =
    (src: Map[FromK, FromV], modifiers: Modifiers) => src.map { case (k, v) =>
      keyTransformer.transform(k, modifiers) -> valueTransformer.transform(v, modifiers)
    }

}

trait GenericInstances {

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

  // $COVERAGE-OFF$
  implicit def cnilCase[From, ToG <: Coproduct, Modifiers <: HList]
  : DerivedTransformer[CNil @@ From, ToG, Modifiers] =
    (_: CNil @@ From, _: Modifiers) => null.asInstanceOf[ToG]
  // $COVERAGE-ON$

  implicit def coproductCase[From, TailFromG <: Coproduct, Label <: Symbol, HeadT, ToG <: Coproduct, Modifiers <: HList]
  (implicit cip: CoproductInstanceProvider[ToG, Label, HeadT, Modifiers],
   tailTransformer: DerivedTransformer[TailFromG @@ From, ToG, Modifiers])
  : DerivedTransformer[@@[FieldType[Label, HeadT] :+: TailFromG, From], ToG, Modifiers] =
    (src: @@[FieldType[Label, HeadT] :+: TailFromG, From], modifiers: Modifiers) =>
      (src : FieldType[Label, HeadT] :+: TailFromG) match {
        case Inl(hd) => cip.provide(hd, modifiers)
        case Inr(tl) => tailTransformer.transform(tag[From](tl), modifiers)
      }

  implicit def gen[From, To, FromG, ToG, Modifiers <: HList]
    (implicit fromLG: LabelledGeneric.Aux[From, FromG],
     toLG: LabelledGeneric.Aux[To, ToG],
     genTransformer: DerivedTransformer[FromG @@ From, ToG, Modifiers]): DerivedTransformer[From, To, Modifiers] = {
    (src: From, modifiers: Modifiers) =>
      toLG.from(genTransformer.transform(tag[From](fromLG.to(src)), modifiers))
  }
}

trait CoproductInstanceProvider[ToG <: Coproduct, Label <: Symbol, T, Modifiers <: HList] {
  def provide(srcInstance: FieldType[Label, T], modifiers: Modifiers): ToG
}

object CoproductInstanceProvider {

  implicit def matchingObjCase[ToG <: Coproduct, ToHList <: HList, Label <: Symbol, T, TargetT, Modifiers <: HList]
    (implicit sel: ops.union.Selector.Aux[ToG, Label, TargetT],
     wit: Witness.Aux[TargetT],
     inj: ops.coproduct.Inject[ToG, FieldType[Label, TargetT]])
  : CoproductInstanceProvider[ToG, Label, T, Modifiers] =
    (_: FieldType[Label, T], _: Modifiers) =>
      inj(field[Label](wit.value))
}
