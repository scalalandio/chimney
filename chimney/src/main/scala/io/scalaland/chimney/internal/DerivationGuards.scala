package io.scalaland.chimney.internal

import scala.reflect.macros.whitebox

trait DerivationGuards {
  this : MacroUtils =>

  val c: whitebox.Context

  import c.universe._

  def bothCaseClasses(from: Type, to: Type): Boolean = {
    from.isCaseClass && to.isCaseClass
  }

  def fromValueClassToType(from: Type, to: Type): Boolean = {
    from.isValueClass && from.valueClassMember.exists(_.returnType =:= to)
  }

  def fromTypeToValueClass(from: Type, to: Type): Boolean = {
    to.isValueClass && to.valueClassMember.exists(_.returnType =:= from)
  }

  def canTryDeriveTransformer(from: Type, to: Type): Boolean = {
    bothCaseClasses(from, to) || fromValueClassToType(from, to) || fromTypeToValueClass(from, to)
  }
}
