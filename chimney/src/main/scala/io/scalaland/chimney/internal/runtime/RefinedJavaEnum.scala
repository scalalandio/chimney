package io.scalaland.chimney.internal.runtime

/** Apparently, `com.mypackage.JavaEnum.Value.type` is not a thing on Scala 2. This means that:
  *
  * withSealedSubtypeHandled[com.mypackage.JavaEnum.Value.type] { value => ... }
  *
  * is ALWAYS treated as:
  *
  * withSealedSubtypeHandled[com.mypackage.JavaEnum] { value => ... }
  *
  * matching on ALL values of `com.mypackage.JavaEnum`. Probably not what you want.
  *
  * While there is `symbol.asTerm.typeSignature` for `JavaEnum.Value` (being aware that it is "Value" and recognizing it
  * in pattern-matching), it will be upcasted to `JavaEnum`. We are only able to read it by reading a whole function's
  * tree:
  *
  * withSealedSubtypeHandled { (value: com.mypackage.JavaEnum.Value.type) => ... }
  *
  * and we are only able to store it for DSL by encoding the name read from the tree with a dedicated type from below.
  *
  * In Scala 3 no such shenanigan are needed.
  */
sealed class RefinedJavaEnum[E, Value <: String]
