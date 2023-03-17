package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal

private[compiletime] trait ConfigurationDefinitionsPlatform extends ConfigurationDefinitions {
  this: DefinitionsPlatform =>

  final override protected def readConfig[
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: TransformerConfig = {
    val sharedFlags = extractTransformerFlags[SharedFlags](TransformerFlags())
    val allFlags = extractTransformerFlags[InstanceFlags](sharedFlags)
    extractTransformerConfig[Cfg](runtimeDataIdx = 0).copy(flags = allFlags)
  }

  private def extractTransformerFlags[Flags <: internal.TransformerFlags: Type](
      defaultFlags: TransformerFlags
  ): TransformerFlags = ???

  private def extractTransformerConfig[Cfg <: internal.TransformerCfg: Type](runtimeDataIdx: Int): TransformerConfig =
    ???
}
