package io.scalaland.chimney.internal.runtime

/** Phantom type used on Scala 3 to scope DSL path extension methods (`matching`, `everyItem`, `everyMapKey`,
  * `everyMapValue`, `matchingSome`, `matchingLeft`, `matchingRight`) so they are only available inside selector lambdas
  * passed to DSL methods like `withFieldConst`, `withFieldRenamed`, etc.
  *
  * On Scala 3, DSL selector parameters use context functions (`ChimneySelector ?=> From => T`) so the compiler
  * automatically provides the `ChimneySelector` evidence within the lambda body, enabling the extension methods. Outside
  * of DSL selectors, these extensions are not in scope.
  *
  * @since 1.8.0
  */
sealed trait ChimneySelector
