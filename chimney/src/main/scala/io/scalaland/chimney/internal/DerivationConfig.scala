package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait DerivationConfig {

  val c: blackbox.Context

  case class TransformerConfig(
      processDefaultValues: Boolean = true,
      enableBeanGetters: Boolean = false,
      enableBeanSetters: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      enableUnsafeOption: Boolean = false,
      constFields: Set[String] = Set.empty,
      computedFields: Set[String] = Set.empty,
      renamedFields: Map[String, String] = Map.empty,
      coproductInstances: Set[(c.Symbol, c.Type)] = Set.empty, // pair: inst type, target type
      transformerDefinitionPrefix: c.Tree = c.universe.EmptyTree,
      definitionScope: Option[(c.Type, c.Type)] = None
  ) {

    def rec: TransformerConfig =
      copy(
        constFields = Set.empty,
        computedFields = Set.empty,
        renamedFields = Map.empty,
        definitionScope = None
      )
  }

  case class PatcherConfig(
      enableIncompletePatches: Boolean = false
  )
}
