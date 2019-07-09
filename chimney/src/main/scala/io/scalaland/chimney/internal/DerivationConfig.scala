package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait DerivationConfig {

  val c: blackbox.Context

  case class Config(processDefaultValues: Boolean = true,
                    enableBeanGetters: Boolean = false,
                    enableBeanSetters: Boolean = false,
                    optionDefaultsToNone: Boolean = false,
                    disableLocalImplicitLookup: Boolean = false,
                    overridenFields: Set[String] = Set.empty,
                    renamedFields: Map[String, String] = Map.empty,
                    coproductInstances: Set[(c.Symbol, c.Type)] = Set.empty, // pair: inst type, target type
                    prefixValName: String = "") {

    def rec: Config =
      copy(
        disableLocalImplicitLookup = false,
        overridenFields = Set.empty,
        renamedFields = Map.empty
      )
  }
}
