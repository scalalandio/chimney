package io.scalaland.chimney.dsl

/** Action to take when some fields of the source are not used in the target.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#unused-source-fields-policies]] for more details
  *
  * @since 1.5.0
  */
sealed abstract class ActionOnUnused

/** Fail the derivation if not all fields of the source are not used. Exceptions can be made using
  * `ignoreUnusedField` overrides.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#unused-source-fields-policies]] for more details
  *
  * @since 1.5.0
  */
case object FailOnUnused extends ActionOnUnused
