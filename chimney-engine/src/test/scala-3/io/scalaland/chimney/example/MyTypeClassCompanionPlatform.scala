package io.scalaland.chimney.example

trait MyTypeClassCompanionPlatform { this: MyTypeClass.type =>

  inline def deriving[From, To] = ${ internal.MyTypeClassMacros.derivingImpl[From, To] }
}
