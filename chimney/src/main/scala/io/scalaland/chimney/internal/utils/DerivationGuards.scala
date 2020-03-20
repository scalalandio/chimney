package io.scalaland.chimney.internal.utils

import scala.reflect.macros.blackbox

trait DerivationGuards {
  this: MacroUtils =>

  val c: blackbox.Context

  import c.universe._

  def isSubtype(from: Type, to: Type): Boolean = {
    from <:< to
  }

  def fromValueClassToType(from: Type, to: Type): Boolean = {
    from.isValueClass && from.valueClassMember.exists(_.returnType =:= to)
  }

  def fromTypeToValueClass(from: Type, to: Type): Boolean = {
    to.isValueClass && to.valueClassMember.exists(_.returnType =:= from)
  }

  def isOption(t: Type): Boolean = {
    t <:< optionTpe
  }

  def bothOptions(from: Type, to: Type): Boolean = {
    isOption(from) && isOption(to)
  }

  def bothEithers(from: Type, to: Type): Boolean = {
    from <:< eitherTpe && to <:< eitherTpe
  }

  def bothOfIterableOrArray(from: Type, to: Type): Boolean = {
    iterableOrArray(from) && iterableOrArray(to)
  }

  def isTuple(to: Type): Boolean =
    Seq(
      typeOf[Tuple1[_]],
      typeOf[Tuple2[_, _]],
      typeOf[Tuple3[_, _, _]],
      typeOf[Tuple4[_, _, _, _]],
      typeOf[Tuple5[_, _, _, _, _]],
      typeOf[Tuple6[_, _, _, _, _, _]],
      typeOf[Tuple7[_, _, _, _, _, _, _]],
      typeOf[Tuple8[_, _, _, _, _, _, _, _]],
      typeOf[Tuple9[_, _, _, _, _, _, _, _, _]],
      typeOf[Tuple10[_, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple11[_, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
      typeOf[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    ).exists(to <:< _)

  def isUnit(tpe: Type): Boolean = {
    tpe <:< typeOf[Unit]
  }

  def destinationCaseClass(to: Type): Boolean = {
    to.isCaseClass
  }

  def destinationJavaBean(to: Type): Boolean = {
    if (to.typeSymbol.isClass) {
      val primaryConstructor = to.typeSymbol.asClass.primaryConstructor
      primaryConstructor.isPublic &&
      primaryConstructor.isMethod &&
      primaryConstructor.asMethod.paramLists == List(Nil) &&
      to.beanSetterMethods.nonEmpty
    } else {
      // $COVERAGE-OFF$
      false
      // $COVERAGE-ON$
    }
  }

  def bothSealedClasses(from: Type, to: Type): Boolean = {
    from.isSealedClass && to.isSealedClass
  }

  def iterableOrArray(t: Type): Boolean = {
    t <:< iterableTpe || t <:< arrayTpe
  }

  def isMap(t: Type): Boolean = {
    t <:< mapTpe
  }

  val optionTpe: Type = typeOf[Option[_]]
  val someTpe: Type = typeOf[Some[_]]
  val noneTpe: Type = typeOf[None.type]
  val eitherTpe: Type = typeOf[Either[_, _]]
  val leftTpe: Type = typeOf[Left[_, _]]
  val rightTpe: Type = typeOf[Right[_, _]]
  val iterableTpe: Type = typeOf[Iterable[_]]
  val arrayTpe: Type = typeOf[Array[_]]
  val mapTpe: Type = typeOf[scala.collection.Map[_, _]]
}
