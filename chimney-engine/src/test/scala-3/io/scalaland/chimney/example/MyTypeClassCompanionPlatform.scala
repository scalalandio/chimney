package io.scalaland.chimney.example

/** Scala-3-specific implementation of derived macro. */
trait MyTypeClassCompanionPlatform { this: MyTypeClass.type =>

  /** Recursive semi-automatic derivation.
    *
    * Would use implicit `MyTypeClass` instances BUT NOT `MyTypeClass.AutoDerived` instances during recursive
    * derivation, which allows saving * CPU cycles and generates more optimal code.
    */
  inline def derived[From, To]: MyTypeClass[From, To] = ${ internal.MyTypeClassMacros.derivingImpl[From, To] }
}

/** Scala-3-specific implementation of derived macro. */
trait MyTypeClassAutoDerivedCompanionPlatform { this: MyTypeClass.AutoDerived.type =>

  /** Recursive automatic derivation.
    *
    * Would use implicit `MyTypeClass` instances BUT NOT `MyTypeClass.AutoDerived` instances during recursive
    * derivation, which allows saving * CPU cycles and generates more optimal code.
    */
  inline given derived[From, To]: MyTypeClass.AutoDerived[From, To] =
    ${ internal.MyTypeClassMacros.derivingImpl[From, To] }
}
