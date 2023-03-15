package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal

private[compiletime] trait ConfigurationDefinitionsPlatform extends ConfigurationDefinitions {
  this: DefinitionsPlatform =>

  import DefinitionsPlatform.*
  import c.universe.{internal as _, Transformer as _, *}

  final protected def readConfigPlatform[
      Cfg <: internal.TransformerCfg: WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: WeakTypeTag,
      ScopeFlags <: internal.TransformerFlags: WeakTypeTag
  ]: TransformerConfig = {
    implicit val Ctg: Type[Cfg] = typeImpl.fromWeak[Cfg]
    implicit val InstanceFlags: Type[InstanceFlags] = typeImpl.fromWeak[InstanceFlags]
    implicit val ScopeFlags: Type[ScopeFlags] = typeImpl.fromWeak[ScopeFlags]
    readConfig[Cfg, InstanceFlags, ScopeFlags]
  }

  final override protected def readConfig[
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: TransformerConfig = {
    val sharedFlags = extractTransformerFlags[SharedFlags](TransformerFlags())
    val allFlags = extractTransformerFlags[InstanceFlags](sharedFlags)
    extractTransformerConfig[Cfg](runtimeDataIdx = 0).copy(flags = allFlags)
  }

  protected[this] type FlagHead <: internal.TransformerFlags.Flag
  protected[this] type FlagTail <: internal.TransformerFlags
  private val enableTC = typeOf[internal.TransformerFlags.Enable[?, ?]].typeConstructor
  private val disableTC = typeOf[internal.TransformerFlags.Disable[?, ?]].typeConstructor
  private val implicitConflictResolutionTC =
    typeOf[internal.TransformerFlags.ImplicitConflictResolution[?]].typeConstructor
  // TODO: this coule be tailrec
  private def extractTransformerFlags[Flag <: internal.TransformerFlags: Type](
      defaultFlags: TransformerFlags
  ): TransformerFlags = {
    val flags = Type[Flag].dealias

    if (flags =:= Type.TransformerFlags.Default) {
      defaultFlags
    } else if (flags.typeConstructor =:= enableTC) {
      val List(h, t) = flags.typeArgs
      implicit val Flag: Type[FlagHead] = typeImpl.fromUntyped(h)
      implicit val Tail: Type[FlagTail] = typeImpl.fromUntyped(t)

      if (Flag.typeConstructor =:= implicitConflictResolutionTC) {
        val preference = Flag.typeArgs.head
        if (preference =:= Type.PreferTotalTransformer) {
          extractTransformerFlags[FlagTail](defaultFlags).setImplicitConflictResolution(
            Some(dsls.PreferTotalTransformer)
          )
        } else if (preference =:= Type.PreferPartialTransformer) {
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
      implicit val Flag: Type[FlagHead] = typeImpl.fromUntyped(h)
      implicit val Tail: Type[FlagTail] = typeImpl.fromUntyped(t)

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

  protected[this] type CfgTail <: internal.TransformerCfg
  private val emptyT = typeOf[internal.TransformerCfg.Empty]
  private val fieldConstTC = typeOf[internal.TransformerCfg.FieldConst[?, ?]].typeConstructor
  private val fieldConstPartialTC = typeOf[internal.TransformerCfg.FieldConstPartial[?, ?]].typeConstructor
  private val fieldComputedTC = typeOf[internal.TransformerCfg.FieldComputed[?, ?]].typeConstructor
  private val fieldComputedPartialTC = typeOf[internal.TransformerCfg.FieldComputedPartial[?, ?]].typeConstructor
  private val fieldRelabelledTC = typeOf[internal.TransformerCfg.FieldRelabelled[?, ?, ?]].typeConstructor
  private val coproductInstanceTC = typeOf[internal.TransformerCfg.CoproductInstance[?, ?, ?]].typeConstructor
  private val coproductInstancePartialTC =
    typeOf[internal.TransformerCfg.CoproductInstancePartial[?, ?, ?]].typeConstructor
  // TODO: this coule be tailrec
  private def extractTransformerConfig[Cfg <: internal.TransformerCfg: Type](runtimeDataIdx: Int): TransformerConfig = {
    val cfgTpe = Type[Cfg].dealias

    if (cfgTpe =:= emptyT) {
      TransformerConfig()
    } else if (cfgTpe.typeConstructor =:= fieldConstTC) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.asStringSingletonType
      implicit val CfgTail: Type[CfgTail] = typeImpl.fromUntyped(rest)
      extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
        .fieldOverride(fieldName, FieldOverride.Const(runtimeDataIdx))
    } else if (cfgTpe.typeConstructor =:= fieldComputedTC) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.asStringSingletonType
      implicit val CfgTail: Type[CfgTail] = typeImpl.fromUntyped(rest)
      extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
        .fieldOverride(fieldName, FieldOverride.Computed(runtimeDataIdx))
    } else if (cfgTpe.typeConstructor =:= fieldRelabelledTC) {
      val List(fieldNameFromT, fieldNameToT, rest) = cfgTpe.typeArgs
      val fieldNameFrom = fieldNameFromT.asStringSingletonType
      val fieldNameTo = fieldNameToT.asStringSingletonType
      implicit val CfgTail: Type[CfgTail] = typeImpl.fromUntyped(rest)
      extractTransformerConfig[CfgTail](runtimeDataIdx)
        .fieldOverride(fieldNameTo, FieldOverride.RenamedFrom(fieldNameFrom))
    } else if (cfgTpe.typeConstructor =:= coproductInstanceTC) {
      val List(instanceType, targetType, rest) = cfgTpe.typeArgs
      implicit val From: Type[Arbitrary] = typeImpl.fromUntyped(instanceType)
      implicit val To: Type[Arbitrary2] = typeImpl.fromUntyped(targetType)
      implicit val CfgTail: Type[CfgTail] = typeImpl.fromUntyped(rest)
      extractTransformerConfig[CfgTail](1 + runtimeDataIdx).coproductInstance[Arbitrary, Arbitrary2](runtimeDataIdx)
    } else if (cfgTpe.typeConstructor =:= fieldConstPartialTC) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.asStringSingletonType
      implicit val Tail: Type[CfgTail] = typeImpl.fromUntyped(rest)
      extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
        .fieldOverride(fieldName, FieldOverride.ConstPartial(runtimeDataIdx))
    } else if (cfgTpe.typeConstructor =:= fieldComputedPartialTC) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.asStringSingletonType
      implicit val Tail: Type[CfgTail] = typeImpl.fromUntyped(rest)
      extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
        .fieldOverride(fieldName, FieldOverride.ComputedPartial(runtimeDataIdx))
    } else if (cfgTpe.typeConstructor =:= coproductInstancePartialTC) {
      val List(instanceType, targetType, rest) = cfgTpe.typeArgs
      implicit val From: Type[Arbitrary] = typeImpl.fromUntyped(instanceType)
      implicit val To: Type[Arbitrary2] = typeImpl.fromUntyped(targetType)
      implicit val Tail: Type[CfgTail] = typeImpl.fromUntyped(rest)
      extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
        .coproductInstancePartial[Arbitrary, Arbitrary2](runtimeDataIdx)
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal transformer config type shape!")
      // $COVERAGE-ON$
    }
  }
}
