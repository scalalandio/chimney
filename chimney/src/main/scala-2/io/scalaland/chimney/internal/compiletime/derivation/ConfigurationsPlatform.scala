package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ConfigurationsPlatform extends Configurations { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  protected object Configurations extends ConfigurationsModule {

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

    protected def extractTransformerConfig[Cfg <: internal.TransformerCfg: Type](
        runtimeDataIdx: Int
    ): TransformerConfig = {
      val cfgTpe = Type[Cfg].tpe.dealias

      if (cfgTpe =:= emptyT) {
        TransformerConfig()
      } else if (cfgTpe.typeConstructor =:= fieldConstTC) {
        val List(fieldNameT, rest) = cfgTpe.typeArgs
        val fieldName = fieldNameT.asStringSingletonType
        implicit val CfgTail: Type[CfgTail] = Type.platformSpecific.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addFieldOverride(fieldName, RuntimeFieldOverride.Const(runtimeDataIdx))
      } else if (cfgTpe.typeConstructor =:= fieldComputedTC) {
        val List(fieldNameT, rest) = cfgTpe.typeArgs
        val fieldName = fieldNameT.asStringSingletonType
        implicit val CfgTail: Type[CfgTail] = Type.platformSpecific.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addFieldOverride(fieldName, RuntimeFieldOverride.Computed(runtimeDataIdx))
      } else if (cfgTpe.typeConstructor =:= fieldConstPartialTC) {
        val List(fieldNameT, rest) = cfgTpe.typeArgs
        val fieldName = fieldNameT.asStringSingletonType
        implicit val Tail: Type[CfgTail] = Type.platformSpecific.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addFieldOverride(fieldName, RuntimeFieldOverride.ConstPartial(runtimeDataIdx))
      } else if (cfgTpe.typeConstructor =:= fieldComputedPartialTC) {
        val List(fieldNameT, rest) = cfgTpe.typeArgs
        val fieldName = fieldNameT.asStringSingletonType
        implicit val Tail: Type[CfgTail] = Type.platformSpecific.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addFieldOverride(fieldName, RuntimeFieldOverride.ComputedPartial(runtimeDataIdx))
      } else if (cfgTpe.typeConstructor =:= fieldRelabelledTC) {
        val List(fieldNameFromT, fieldNameToT, rest) = cfgTpe.typeArgs
        val fieldNameFrom = fieldNameFromT.asStringSingletonType
        val fieldNameTo = fieldNameToT.asStringSingletonType
        implicit val CfgTail: Type[CfgTail] = Type.platformSpecific.fromUntyped(rest)
        extractTransformerConfig[CfgTail](runtimeDataIdx)
          .addFieldOverride(fieldNameTo, RuntimeFieldOverride.RenamedFrom(fieldNameFrom))
      } else if (cfgTpe.typeConstructor =:= coproductInstanceTC) {
        val List(instanceType, targetType, rest) = cfgTpe.typeArgs
        val From: Type[?] = Type.platformSpecific.fromUntyped(instanceType)
        val To: Type[?] = Type.platformSpecific.fromUntyped(targetType)
        implicit val CfgTail: Type[CfgTail] = Type.platformSpecific.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addCoproductInstance(
            From.asExistential,
            To.asExistential,
            RuntimeCoproductOverride.CoproductInstance(runtimeDataIdx)
          )
      } else if (cfgTpe.typeConstructor =:= coproductInstancePartialTC) {
        val List(instanceType, targetType, rest) = cfgTpe.typeArgs
        val From: Type[?] = Type.platformSpecific.fromUntyped(instanceType)
        val To: Type[?] = Type.platformSpecific.fromUntyped(targetType)
        implicit val Tail: Type[CfgTail] = Type.platformSpecific.fromUntyped(rest)
        extractTransformerConfig[CfgTail](1 + runtimeDataIdx)
          .addCoproductInstance(
            From.asExistential,
            To.asExistential,
            RuntimeCoproductOverride.CoproductInstancePartial(runtimeDataIdx)
          )
      } else {
        // $COVERAGE-OFF$
        reportError("Bad internal transformer config type shape!")
        // $COVERAGE-ON$
      }
    }
  }

  // TODO: move to Type.platformSpecific ?
  implicit private class StringSingletonTypeOps(private val tpe: c.Type) {

    /** Assumes that this `tpe` is String singleton type and extracts its value */
    def asStringSingletonType: String = tpe
      .asInstanceOf[scala.reflect.internal.Types#UniqueConstantType]
      .value
      .value
      .asInstanceOf[String]
  }
}
