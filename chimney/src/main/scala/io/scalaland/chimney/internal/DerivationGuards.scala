package io.scalaland.chimney.internal

import scala.reflect.macros.whitebox

trait DerivationGuards {
  this: MacroUtils =>

  val c: whitebox.Context

  import c.universe._

  def isSubtype(from: Type, to: Type): Boolean = {
    from <:< to
  }

  def bothCaseClasses(from: Type, to: Type): Boolean = {
    from.isCaseClass && to.isCaseClass
  }

  def fromValueClassToType(from: Type, to: Type): Boolean = {
    from.isValueClass && from.valueClassMember.exists(_.returnType =:= to)
  }

  def fromTypeToValueClass(from: Type, to: Type): Boolean = {
    to.isValueClass && to.valueClassMember.exists(_.returnType =:= from)
  }

  def bothMaps(from: Type, to: Type): Boolean = {
    from <:< mapT && to <:< mapT
  }

  def bothOfTraversableOrArray(from: Type, to: Type): Boolean = {
    traversableOrArray(from) && traversableOrArray(to)
  }

  def canTryDeriveTransformer(from: Type, to: Type): Boolean = {
    isSubtype(from, to) ||
    bothCaseClasses(from, to) ||
    fromValueClassToType(from, to) ||
    fromTypeToValueClass(from, to) ||
    bothMaps(from, to) ||
    bothOfTraversableOrArray(from, to)
  }

  def traversableOrArray(t: Type): Boolean = {
    t <:< traversableT || t <:< arrayT
  }

  val traversableT: Type = typeOf[Traversable[_]]
  val arrayT: Type = typeOf[Array[_]]
  val mapT: Type = typeOf[scala.collection.Map[_, _]]
}
