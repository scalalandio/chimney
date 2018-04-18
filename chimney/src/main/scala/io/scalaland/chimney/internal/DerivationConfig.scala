package io.scalaland.chimney.internal

import scala.reflect.macros.whitebox

trait DerivationConfig {

  val c: whitebox.Context

  case class Config(disableDefaultValues: Boolean, overridenFields: Set[String], prefixValName: String = "")
}
