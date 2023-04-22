package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal

private[compiletime] trait ConfigurationsPlatform extends Configurations { this: DefinitionsPlatform =>

  import quotes.*
  import quotes.reflect.*

  protected object configurationsImpl extends ConfigurationDefinitionsImpl {

    final override def readTransformerConfig[
        From: Type,
        To: Type,
        Cfg <: internal.TransformerCfg: Type,
        InstanceFlags <: internal.TransformerFlags: Type,
        SharedFlags <: internal.TransformerFlags: Type
    ]: TransformerConfig = {
      val sharedFlags = extractTransformerFlags[SharedFlags](TransformerFlags())
      val allFlags = extractTransformerFlags[InstanceFlags](sharedFlags)
      extractTransformerConfig[From, To, Cfg](runtimeDataIdx = 0).copy(flags = allFlags)
    }

    private def extractTransformerFlags[Flag <: internal.TransformerFlags: Type](
        defaultFlags: TransformerFlags
    ): TransformerFlags = {
      val flags = TypeRepr.of[Flag].dealias

      flags.asType match {
        case '[internal.TransformerFlags.Default] =>
          defaultFlags
        case '[internal.TransformerFlags.Enable[flag, flags]] =>
          val flagsRest = extractTransformerFlags[flags](defaultFlags)
          Type[flag] match
            case '[internal.TransformerFlags.ImplicitConflictResolution[dsls.PreferTotalTransformer.type]] =>
              flagsRest.setImplicitConflictResolution(Some(dsls.PreferTotalTransformer))
            case '[internal.TransformerFlags.ImplicitConflictResolution[dsls.PreferPartialTransformer.type]] =>
              flagsRest.setImplicitConflictResolution(Some(dsls.PreferPartialTransformer))
            case _ =>
              flagsRest.setBoolFlag[flag](value = true)
        case '[internal.TransformerFlags.Disable[flag, flags]] =>
          val flagsRest = extractTransformerFlags[flags](defaultFlags)
          Type[flag] match
            case '[internal.TransformerFlags.ImplicitConflictResolution[?]] =>
              flagsRest.setImplicitConflictResolution(None)
            case _ =>
              flagsRest.setBoolFlag[flag](value = false)
        case _ =>
          reportError("Bad internal transformer flags type shape!")
      }
    }

    private def extractTransformerConfig[From: Type, To: Type, Cfg <: internal.TransformerCfg: Type](
        runtimeDataIdx: Int
    ): TransformerConfig =
      TransformerConfig()
  }
}
