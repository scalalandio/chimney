package io.scalaland.chimney.example

import scala.language.experimental.macros

/** Scala-2-specific implementation of derived macro. */
trait MyTypeClassCompanionPlatform { this: MyTypeClass.type =>

  /** Recursive semi-automatic derivation.
    *
    * Would use implicit `MyTypeClass` instances BUT NOT `MyTypeClass.AutoDerived` instances during recursive
    * derivation, which allows saving * CPU cycles and generates more optimal code.
    */
  def derived[From, To]: MyTypeClass[From, To] = macro internal.MyTypeClassMacros.derivingImpl[From, To]
}

/** Scala-2-specific implementation of derived macro. */
trait MyTypeClassAutoDerivedCompanionPlatform { this: MyTypeClass.AutoDerived.type =>

  /** Recursive automatic derivation.
    *
    * Would use implicit `MyTypeClass` instances BUT NOT `MyTypeClass.AutoDerived` instances during recursive
    * derivation, which allows saving * CPU cycles and generates more optimal code.
    */
  def derived[From, To]: MyTypeClass.AutoDerived[From, To] = macro internal.MyTypeClassMacros.derivingImpl[From, To]
}
