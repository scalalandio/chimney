package io.scalaland.chimney.internal.runtime

/** Apparently, `com.mypackage.JavaEnum.Value.type` is not a thing on Scala 2. At least when it comes to macros and
  * `c.WeakTypeTag`.
  *
  * While there is `symbol.asTerm.typeSignature` for `JavaEnum.Value` (being aware that it "Value"), it will be upcasted
  * to `JavaEnum`. We are only able to read it by reading a whole function tree, and we are only able to store it
  * for DSL by encoding the name read from the tree in a dedicate type.
  *
  * In Scala 3 no such shenanigan are needed.
  */
sealed class RefinedJavaEnum[E, Value <: String]
