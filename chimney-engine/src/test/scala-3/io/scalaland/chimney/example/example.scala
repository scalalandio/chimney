package io.scalaland.chimney.example

extension [From](source: From) {

  /** Privides the extension method on Scala 3.
    *
    * Would allow usage of user-provided `MyTypeClass` falling back on autoderived `MyTypeClass.AutoDerived` WITHOUT the
    * overhead normally associated with automatic derivation.
    */
  def convertTo[To](using typeClass: MyTypeClass.AutoDerived[From, To]): To =
    typeClass.convert(source)
}
