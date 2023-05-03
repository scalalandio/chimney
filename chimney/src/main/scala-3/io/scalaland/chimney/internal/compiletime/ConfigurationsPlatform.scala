package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal

private[compiletime] trait ConfigurationsPlatform extends Configurations { this: DefinitionsPlatform =>

  import quotes.*
  import quotes.reflect.*

  protected object configurationsImpl extends ConfigurationDefinitionsImpl {

    final override def readTransformerConfig[
        Cfg <: internal.TransformerCfg: Type,
        InstanceFlags <: internal.TransformerFlags: Type,
        ImplicitScopeFlags <: internal.TransformerFlags: Type
    ]: TransformerConfig = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](TransformerFlags())
      val allFlags = extractTransformerFlags[InstanceFlags](implicitScopeFlags)
      extractTransformerConfig[Cfg](runtimeDataIdx = 0).copy(flags = allFlags)
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
          println(s"Bad tpe: ${Type.prettyPrint[Flag]}")
          reportError(s"Bad internal transformer flags type shape!  ${Type.prettyPrint[Flag]}")
      }
    }

    private def extractTransformerConfig[Cfg <: internal.TransformerCfg: Type](
        runtimeDataIdx: Int
    ): TransformerConfig = {
      val cfgTpe = TypeRepr.of[Cfg].dealias

      cfgTpe.asType match {
        case '[internal.TransformerCfg.Empty] =>
          TransformerConfig()
        case '[internal.TransformerCfg.FieldConst[fieldNameT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addFieldOverride(Type[fieldNameT].asStringSingletonType, RuntimeFieldOverride.Const(runtimeDataIdx))
        case '[internal.TransformerCfg.FieldComputed[fieldNameT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addFieldOverride(Type[fieldNameT].asStringSingletonType, RuntimeFieldOverride.Computed(runtimeDataIdx))
        case '[internal.TransformerCfg.FieldConstPartial[fieldNameT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addFieldOverride(Type[fieldNameT].asStringSingletonType, RuntimeFieldOverride.ConstPartial(runtimeDataIdx))
        case '[internal.TransformerCfg.FieldComputedPartial[fieldNameT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addFieldOverride(
              Type[fieldNameT].asStringSingletonType,
              RuntimeFieldOverride.ComputedPartial(runtimeDataIdx)
            )
        case '[internal.TransformerCfg.FieldRelabelled[fieldNameFromT, fieldNameToT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addFieldOverride(
              Type[fieldNameFromT].asStringSingletonType,
              RuntimeFieldOverride.RenamedFrom(Type[fieldNameToT].asStringSingletonType)
            )
        case '[internal.TransformerCfg.CoproductInstance[instanceT, targetT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addCoproductInstance(
              ComputedType(Type[instanceT]),
              ComputedType(Type[targetT]),
              RuntimeCoproductOverride.CoproductInstance(runtimeDataIdx)
            )
        case '[internal.TransformerCfg.CoproductInstancePartial[instanceT, targetT, cfgTailT]] =>
          extractTransformerConfig[cfgTailT](1 + runtimeDataIdx)
            .addCoproductInstance(
              ComputedType(Type[instanceT]),
              ComputedType(Type[targetT]),
              RuntimeCoproductOverride.CoproductInstancePartial(runtimeDataIdx)
            )
        case _ =>
          reportError(
            s"Bad internal transformer config type shape!  ${Type.prettyPrint[Cfg]} dealiased to ${cfgTpe.show(using Printer.TypeReprAnsiCode)}"
          )
      }
    }
  }
}
