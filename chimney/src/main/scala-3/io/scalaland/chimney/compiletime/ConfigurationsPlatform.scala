package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal

private[compiletime] trait ConfigurationsPlatform extends Configurations { this: DefinitionsPlatform =>

  protected object configurationsImpl extends ConfigurationDefinitionsImpl {

    final override def extractRuntimeConfiguration[From: Type, ToField: Type](
        runtimeConfiguration: FieldOverride.RuntimeConfiguration,
        runtimeDataStore: Expr[dsls.TransformerDefinitionCommons.RuntimeDataStore]
    ): FieldOverride.ValueSource[From, ToField] = ???

    final override def readTransformerConfig[
        From: Type,
        To: Type,
        Cfg <: internal.TransformerCfg: Type,
        InstanceFlags <: internal.TransformerFlags: Type,
        SharedFlags <: internal.TransformerFlags: Type
    ]: TransformerConfig[From, To] = {
      val sharedFlags = extractTransformerFlags[SharedFlags](TransformerFlags())
      val allFlags = extractTransformerFlags[InstanceFlags](sharedFlags)
      extractTransformerConfig[From, To, Cfg](runtimeDataIdx = 0).copy(flags = allFlags)
    }

    private def extractTransformerFlags[Flag <: internal.TransformerFlags: Type](
        defaultFlags: TransformerFlags
    ): TransformerFlags = ???

    private def extractTransformerConfig[From: Type, To: Type, Cfg <: internal.TransformerCfg: Type](
        runtimeDataIdx: Int
    ): TransformerConfig[From, To] =
      ???
  }
}
