package io.scalaland.chimney.integrations

@FunctionalInterface
trait DefaultValue[Value] {

  def provide(): Value
}
