package io.scalaland.chimney.example

/** Type class that users should use to define their own custom behavior or returned by `MyTypeClass.derived[From, To]`
  * recursive semi-automatic derivation.
  */
trait MyTypeClass[From, To] extends MyTypeClass.AutoDerived[From, To] {

  def convert(from: From): To
}
object MyTypeClass extends MyTypeClassCompanionPlatform {

  /** Type class that extension method should use to prioritize user-defined instances with a fallback to recursively
    * autoderived instance.
    *
    * Instances defined as `AutoDerived` but NOT as `MyTypeClass` will NOT be looked for by macros, which allows saving
    * CPU cycles and generates more optimal code.
    */
  trait AutoDerived[From, To] {

    def convert(from: From): To
  }
  object AutoDerived extends MyTypeClassAutoDerivedCompanionPlatform
}
