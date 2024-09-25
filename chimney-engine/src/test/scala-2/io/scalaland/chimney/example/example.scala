package io.scalaland.chimney

package object example {

  implicit class ConvertOps[From](private val source: From) extends AnyVal {

    /** Privides the extension method on Scala 2.
      *
      * Would allow usage of user-provided `MyTypeClass` falling back on autoderived `MyTypeClass.AutoDerived` WITHOUT
      * the overhead normally associated with automatic derivation.
      */
    def convertTo[To](implicit typeClass: MyTypeClass.AutoDerived[From, To]): To =
      typeClass.convert(source)
  }
}
