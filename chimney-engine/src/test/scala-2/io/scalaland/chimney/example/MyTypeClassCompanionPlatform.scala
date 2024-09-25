package io.scalaland.chimney.example

import scala.language.experimental.macros

trait MyTypeClassCompanionPlatform { this: MyTypeClass.type =>

  def deriving[From, To]: MyTypeClass[From, To] = macro internal.MyTypeClassMacros.derivingImpl[From, To]
}
