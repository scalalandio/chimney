package io.scalaland.chimney.example.internal

import io.scalaland.chimney.example.MyTypeClass

import scala.quoted.*

class MyTypeClassMacros(q: Quotes) extends MyTypeClassDerivationPlatform(q)
object MyTypeClassMacros {

  // Scala 3 expect all macros to be methods in objects, no Scala 2's macro-bundle-like equivalents, so we have to
  // instantiate a class and call it ourselves
  def derivingImpl[From: Type, To: Type](using q: Quotes): Expr[MyTypeClass[From, To]] =
    new MyTypeClassMacros(q).myTypeClassDerivation[From, To]
}
