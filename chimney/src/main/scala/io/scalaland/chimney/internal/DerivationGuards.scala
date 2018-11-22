package io.scalaland.chimney.internal

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

  def bothOptions(from: Type, to: Type): Boolean = {
    from <:< optionTpe && to <:< optionTpe
  }

  def bothEithers(from: Type, to: Type): Boolean = {
    from <:< eitherTpe && to <:< eitherTpe
  }

  def bothMaps(from: Type, to: Type): Boolean = {
    from <:< mapTpe && to <:< mapTpe
  }

  def bothOfTraversableOrArray(from: Type, to: Type): Boolean = {
    iterableOrArray(from) && iterableOrArray(to)
  }

  def destinationCaseClass(to: Type): Boolean = {
    to.isCaseClass
  }

  def destinationJavaBean(to: Type): Boolean = {
    if (to.typeSymbol.isClass) {
      val primaryConstructor = to.typeSymbol.asClass.primaryConstructor
      primaryConstructor.isPublic && primaryConstructor.asMethod.paramLists == List(Nil) && to.beanSetterMethods.nonEmpty
    } else {
      false
    }
  }

  def bothSealedClasses(from: Type, to: Type): Boolean = {
    from.isSealedClass && to.isSealedClass
  }

  def canTryDeriveTransformer(from: Type, to: Type): Boolean = {
    isSubtype(from, to) ||
    fromValueClassToType(from, to) ||
    fromTypeToValueClass(from, to) ||
    bothOptions(from, to) ||
    bothEithers(from, to) ||
    bothMaps(from, to) ||
    bothOfTraversableOrArray(from, to) ||
    destinationCaseClass(to) ||
    destinationJavaBean(to) ||
    bothSealedClasses(from, to)
  }

  def iterableOrArray(t: Type): Boolean = {
    t <:< iterableTpe || t <:< arrayTpe
  }

  val optionTpe: Type = typeOf[Option[_]]
  val someTpe: Type = typeOf[Some[_]]
  val noneTpe: Type = typeOf[None.type]
  val eitherTpe: Type = typeOf[Either[_, _]]
  val leftTpe: Type = typeOf[Left[_, _]]
  val rightTpe: Type = typeOf[Right[_, _]]
  val mapTpe: Type = typeOf[scala.collection.Map[_, _]]
  val iterableTpe: Type = typeOf[Iterable[_]]
  val arrayTpe: Type = typeOf[Array[_]]
}
