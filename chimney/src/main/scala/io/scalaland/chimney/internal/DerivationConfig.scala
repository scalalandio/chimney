package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait DerivationConfig {

  val c: blackbox.Context

  case class Config(disableDefaultValues: Boolean = false,
                    overridenFields: Set[String] = Set.empty,
                    renamedFields: Map[String, String] = Map.empty,
                    coproductInstances: Set[(c.Symbol, c.Type)] = Set.empty, // pair: inst type, target type
                    prefixValName: String = "") {

    def rec: Config =
      copy(overridenFields = Set.empty, renamedFields = Map.empty)
  }
}
