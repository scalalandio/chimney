package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.ImplicitTransformerPreference
import io.scalaland.chimney.internal
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.TransformerCfg

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Configurations { this: Definitions =>

  final protected case class TransformerFlags(
      processDefaultValues: Boolean = false,
      beanSetters: Boolean = false,
      beanGetters: Boolean = false,
      methodAccessors: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      implicitConflictResolution: Option[ImplicitTransformerPreference] = None
  ) {

    def setBoolFlag[Flag <: internal.TransformerFlags.Flag: Type](value: Boolean): TransformerFlags =
      if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.DefaultValues) {
        copy(processDefaultValues = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.BeanSetters) {
        copy(beanSetters = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.BeanGetters) {
        copy(beanGetters = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.MethodAccessors) {
        copy(methodAccessors = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.OptionDefaultsToNone) {
        copy(optionDefaultsToNone = value)
      } else {
        // $COVERAGE-OFF$
        reportError(s"Invalid transformer flag type: ${Type[Flag]}!")
        // $COVERAGE-ON$
      }

    def setImplicitConflictResolution(preference: Option[ImplicitTransformerPreference]): TransformerFlags =
      copy(implicitConflictResolution = preference)
  }

  sealed abstract class RuntimeFieldOverride(val needValueLevelAccess: Boolean) extends Product with Serializable
  object RuntimeFieldOverride {
    final case class Const(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class ConstPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class Computed(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class ComputedPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class RenamedFrom(sourceName: String) extends RuntimeFieldOverride(false)
  }

  sealed abstract class RuntimeCoproductOverride extends Product with Serializable
  object RuntimeCoproductOverride {
    final case class CoproductInstance(runtimeDataIdx: Int) extends RuntimeCoproductOverride
    final case class CoproductInstancePartial(runtimeDataIdx: Int) extends RuntimeCoproductOverride
  }

  final protected case class TransformerConfig(
      flags: TransformerFlags = TransformerFlags(),
      fieldOverrides: Map[String, RuntimeFieldOverride] = Map.empty,
      coproductOverrides: Map[(ComputedType, ComputedType), RuntimeCoproductOverride] = Map.empty,
      preventResolutionForTypes: Option[(ComputedType, ComputedType)] = None,
      legacy: TransformerConfig.LegacyData = TransformerConfig.LegacyData() // TODO: temporary
  ) {

    def prepareForRecursiveCall: TransformerConfig =
      copy(
        preventResolutionForTypes = None,
        fieldOverrides = Map.empty,
        legacy = legacy.copy(definitionScope = None)
      )

    def addFieldOverride(fieldName: String, fieldOverride: RuntimeFieldOverride): TransformerConfig = {
      copy(fieldOverrides = fieldOverrides + (fieldName -> fieldOverride))
    }

    def addCoproductInstance(
        instanceType: ComputedType,
        targetType: ComputedType,
        coproductOverride: RuntimeCoproductOverride
    ): TransformerConfig = {
      copy(coproductOverrides = coproductOverrides + ((instanceType, targetType) -> coproductOverride))
    }

    def withDefinitionScope(defScope: (ComputedType, ComputedType)): TransformerConfig = {
      copy(preventResolutionForTypes = Some(defScope), legacy = legacy.copy(definitionScope = Some(defScope)))
    }
  }

  object TransformerConfig {

    type UpdateCfg[_ <: TransformerCfg]

    // TODO: for creating TransformerConfig for old macros in Scala 2 until everything is migrated
    final case class LegacyData(
        transformerDefinitionPrefix: Expr[dsls.TransformerDefinitionCommons[UpdateCfg]] =
          null.asInstanceOf[Expr[dsls.TransformerDefinitionCommons[UpdateCfg]]],
        definitionScope: Option[(ComputedType, ComputedType)] = None
    )
  }

  protected def configurationsImpl: ConfigurationDefinitionsImpl
  protected trait ConfigurationDefinitionsImpl {

    def readTransformerConfig[
        Cfg <: internal.TransformerCfg: Type,
        InstanceFlags <: internal.TransformerFlags: Type,
        SharedFlags <: internal.TransformerFlags: Type
    ]: TransformerConfig
  }
}