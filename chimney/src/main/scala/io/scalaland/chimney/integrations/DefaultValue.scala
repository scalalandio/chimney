package io.scalaland.chimney.integrations

/** Tells Chimney how to provide default value of type `Value` if flag allows them but field does not define it.
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#custom-default-values]] for more details
  *
  * @tparam Value
  *   type of default value
  *
  * @since 1.2.0
  */
@FunctionalInterface
trait DefaultValue[Value] {

  /** Provide the default value. */
  def provide(): Value
}
