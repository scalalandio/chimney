package io.scalaland.chimney.example.internal

import io.scalaland.chimney.example.MyTypeClass

import scala.reflect.macros.blackbox

// Scala 2 macro bundle
class MyTypeClassMacros(val c: blackbox.Context) extends MyTypeClassDerivationPlatform {

  // Scala 2 is kinda unaware during macro expansion that myTypeClassDerivation takes c.WeakTypeTag, and we need to
  // point it out for it, explicitly
  def derivingImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[MyTypeClass[From, To]] =
    myTypeClassDerivation[From, To]
}
