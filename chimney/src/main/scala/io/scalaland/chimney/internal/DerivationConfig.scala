package io.scalaland.chimney.internal

import scala.reflect.macros.whitebox

trait DerivationConfig {

  val c: whitebox.Context

  case class Config(disableDefaultValues: Boolean = false,
                    overridenFields: Set[String] = Set.empty,
                    renamedFields: Map[String, String] = Map.empty,
                    prefixValName: String = "") {

    def rec: Config =
      copy(overridenFields = Set.empty, renamedFields = Map.empty)
  }
}
