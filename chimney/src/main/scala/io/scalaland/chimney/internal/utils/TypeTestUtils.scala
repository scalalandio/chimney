package io.scalaland.chimney.internal.utils

import scala.reflect.macros.blackbox
import scala.collection.compat.*

trait TypeTestUtils extends MacroUtils {

  val c: blackbox.Context

  import c.universe.*

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

  def fromOptionToNonOption(from: Type, to: Type): Boolean = {
    isOption(from) && !isOption(to) && from.typeArgs.sizeIs == 1
  }

  def isTuple(to: Type): Boolean =
    Seq(
      typeOf[Tuple1[?]],
      typeOf[Tuple2[?, ?]],
      typeOf[Tuple3[?, ?, ?]],
      typeOf[Tuple4[?, ?, ?, ?]],
      typeOf[Tuple5[?, ?, ?, ?, ?]],
      typeOf[Tuple6[?, ?, ?, ?, ?, ?]],
      typeOf[Tuple7[?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple8[?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]],
      typeOf[Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
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

  val optionTpe: Type = typeOf[Option[?]]
  val someTpe: Type = typeOf[Some[?]]
  val noneTpe: Type = typeOf[None.type]
  val eitherTpe: Type = typeOf[Either[?, ?]]
  val leftTpe: Type = typeOf[Left[?, ?]]
  val rightTpe: Type = typeOf[Right[?, ?]]
  val iterableTpe: Type = typeOf[Iterable[?]]
  val arrayTpe: Type = typeOf[Array[?]]
  val mapTpe: Type = typeOf[scala.collection.Map[?, ?]]
}
