package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal

private[compiletime] trait ConfigurationsPlatform extends Configurations { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

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

    protected type FlagHead <: internal.TransformerFlags.Flag
    protected type FlagTail <: internal.TransformerFlags
    private val enableTC = typeOf[internal.TransformerFlags.Enable[?, ?]].typeConstructor
    private val disableTC = typeOf[internal.TransformerFlags.Disable[?, ?]].typeConstructor
    private val implicitConflictResolutionTC =
      typeOf[internal.TransformerFlags.ImplicitConflictResolution[?]].typeConstructor

    private def extractTransformerFlags[Flag <: internal.TransformerFlags: Type](
        defaultFlags: TransformerFlags
    ): TransformerFlags = {
      val flags = Type[Flag].dealias

      if (flags =:= ChimneyType.TransformerFlags.Default) {
        defaultFlags
      } else if (flags.typeConstructor =:= enableTC) {
        val List(h, t) = flags.typeArgs
        implicit val Flag: Type[FlagHead] = typeUtils.fromUntyped(h)
        implicit val Tail: Type[FlagTail] = typeUtils.fromUntyped(t)

        if (Flag.typeConstructor =:= implicitConflictResolutionTC) {
          val preference = Flag.typeArgs.head
          if (preference =:= ChimneyType.PreferTotalTransformer) {
            extractTransformerFlags[FlagTail](defaultFlags).setImplicitConflictResolution(
              Some(dsls.PreferTotalTransformer)
            )
          } else if (preference =:= ChimneyType.PreferPartialTransformer) {
            extractTransformerFlags[FlagTail](defaultFlags).setImplicitConflictResolution(
              Some(dsls.PreferPartialTransformer)
            )
          } else {
            // $COVERAGE-OFF$
            c.abort(c.enclosingPosition, "Invalid implicit conflict resolution preference type!!")
            // $COVERAGE-ON$
          }
        } else {
          extractTransformerFlags[FlagTail](defaultFlags).setBoolFlag[FlagHead](value = true)
        }
      } else if (flags.typeConstructor =:= disableTC) {
        val List(h, t) = flags.typeArgs
        implicit val Flag: Type[FlagHead] = typeUtils.fromUntyped(h)
        implicit val Tail: Type[FlagTail] = typeUtils.fromUntyped(t)

        if (flags.typeConstructor =:= implicitConflictResolutionTC) {
          extractTransformerFlags[FlagTail](defaultFlags).setImplicitConflictResolution(None)
        } else {
          extractTransformerFlags[FlagTail](defaultFlags).setBoolFlag[FlagHead](value = false)
        }
      } else {
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, "Bad internal transformer flags type shape!")
        // $COVERAGE-ON$
      }
    }

    protected type CfgTail <: internal.TransformerCfg
    private val emptyT = typeOf[internal.TransformerCfg.Empty]
    private val fieldConstTC = typeOf[internal.TransformerCfg.FieldConst[?, ?]].typeConstructor
    private val fieldConstPartialTC = typeOf[internal.TransformerCfg.FieldConstPartial[?, ?]].typeConstructor
    private val fieldComputedTC = typeOf[internal.TransformerCfg.FieldComputed[?, ?]].typeConstructor
    private val fieldComputedPartialTC = typeOf[internal.TransformerCfg.FieldComputedPartial[?, ?]].typeConstructor
    private val fieldRelabelledTC = typeOf[internal.TransformerCfg.FieldRelabelled[?, ?, ?]].typeConstructor
    private val coproductInstanceTC = typeOf[internal.TransformerCfg.CoproductInstance[?, ?, ?]].typeConstructor
    private val coproductInstancePartialTC =
      typeOf[internal.TransformerCfg.CoproductInstancePartial[?, ?, ?]].typeConstructor

    // TODO: adjust for new config type
    // TODO: this coule be tailrec
    private def extractTransformerConfig[Cfg <: internal.TransformerCfg: Type](
        runtimeDataIdx: Int
    ): TransformerConfig = {
      val cfgTpe = Type[Cfg].dealias

      if (cfgTpe =:= emptyT) {
        TransformerConfig()
      } else if (cfgTpe.typeConstructor =:= fieldConstTC) {
        val List(fieldNameT, rest) = cfgTpe.typeArgs
        val fieldName = fieldNameT.asStringSingletonType
        implicit val CfgTail: Type[CfgTail] = typeUtils.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addFieldOverride(fieldName, RuntimeFieldOverride.Const(runtimeDataIdx))
      } else if (cfgTpe.typeConstructor =:= fieldComputedTC) {
        val List(fieldNameT, rest) = cfgTpe.typeArgs
        val fieldName = fieldNameT.asStringSingletonType
        implicit val CfgTail: Type[CfgTail] = typeUtils.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addFieldOverride(fieldName, RuntimeFieldOverride.Computed(runtimeDataIdx))
      } else if (cfgTpe.typeConstructor =:= fieldConstPartialTC) {
        val List(fieldNameT, rest) = cfgTpe.typeArgs
        val fieldName = fieldNameT.asStringSingletonType
        implicit val Tail: Type[CfgTail] = typeUtils.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addFieldOverride(fieldName, RuntimeFieldOverride.ConstPartial(runtimeDataIdx))
      } else if (cfgTpe.typeConstructor =:= fieldComputedPartialTC) {
        val List(fieldNameT, rest) = cfgTpe.typeArgs
        val fieldName = fieldNameT.asStringSingletonType
        implicit val Tail: Type[CfgTail] = typeUtils.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addFieldOverride(fieldName, RuntimeFieldOverride.ComputedPartial(runtimeDataIdx))
      } else if (cfgTpe.typeConstructor =:= fieldRelabelledTC) {
        val List(fieldNameFromT, fieldNameToT, rest) = cfgTpe.typeArgs
        val fieldNameFrom = fieldNameFromT.asStringSingletonType
        val fieldNameTo = fieldNameToT.asStringSingletonType
        implicit val CfgTail: Type[CfgTail] = typeUtils.fromUntyped(rest)
        extractTransformerConfig[CfgTail](runtimeDataIdx)
          .addFieldOverride(fieldNameTo, RuntimeFieldOverride.RenamedFrom(fieldNameFrom))
      } else if (cfgTpe.typeConstructor =:= coproductInstanceTC) {
        val List(instanceType, targetType, rest) = cfgTpe.typeArgs
        val From: Type[?] = typeUtils.fromUntyped(instanceType)
        val To: Type[?] = typeUtils.fromUntyped(targetType)
        implicit val CfgTail: Type[CfgTail] = typeUtils.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addCoproductInstance(
            ComputedType(From),
            ComputedType(To),
            RuntimeCoproductOverride.CoproductInstance(runtimeDataIdx)
          )
      } else if (cfgTpe.typeConstructor =:= coproductInstancePartialTC) {
        val List(instanceType, targetType, rest) = cfgTpe.typeArgs
        val From: Type[?] = typeUtils.fromUntyped(instanceType)
        val To: Type[?] = typeUtils.fromUntyped(targetType)
        implicit val Tail: Type[CfgTail] = typeUtils.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addCoproductInstance(
            ComputedType(From),
            ComputedType(To),
            RuntimeCoproductOverride.CoproductInstancePartial(runtimeDataIdx)
          )
      } else {
        // $COVERAGE-OFF$
        reportError("Bad internal transformer config type shape!")
        // $COVERAGE-ON$
      }
    }
  }
}
