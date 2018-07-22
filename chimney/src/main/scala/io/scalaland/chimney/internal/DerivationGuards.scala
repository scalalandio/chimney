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
    traversableOrArray(from) && traversableOrArray(to)
  }

  def destinationCaseClass(to: Type): Boolean = {
    to.isCaseClass
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
    bothSealedClasses(from, to)
  }

  def traversableOrArray(t: Type): Boolean = {
    t <:< traversableTpe || t <:< arrayTpe
  }

  val optionTpe: Type = typeOf[Option[_]]
  val someTpe: Type = typeOf[Some[_]]
  val noneTpe: Type = typeOf[None.type]
  val eitherTpe: Type = typeOf[Either[_, _]]
  val leftTpe: Type = typeOf[Left[_, _]]
  val rightTpe: Type = typeOf[Right[_, _]]
  val mapTpe: Type = typeOf[scala.collection.Map[_, _]]
  val traversableTpe: Type = typeOf[Traversable[_]]
  val arrayTpe: Type = typeOf[Array[_]]
}
