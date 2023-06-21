package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.dsl.ImplicitTransformerPreference
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.compiletime.Definitions

private[compiletime] trait Configurations { this: Definitions =>

  final protected case class TransformerFlags(
      processDefaultValues: Boolean = false,
      beanSetters: Boolean = false,
      beanGetters: Boolean = false,
      methodAccessors: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      implicitConflictResolution: Option[ImplicitTransformerPreference] = None,
      displayMacrosLogging: Boolean = false
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
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.MacrosLogging) {
        copy(displayMacrosLogging = value)
      } else {
        // $COVERAGE-OFF$
        reportError(s"Invalid transformer flag type: ${Type[Flag]}!")
        // $COVERAGE-ON$
      }

    def setImplicitConflictResolution(preference: Option[ImplicitTransformerPreference]): TransformerFlags =
      copy(implicitConflictResolution = preference)

    override def toString: String = s"Flags(${Vector(
        if (processDefaultValues) Vector("processDefaultValues") else Vector.empty,
        if (beanSetters) Vector("beanSetters") else Vector.empty,
        if (beanGetters) Vector("beanGetters") else Vector.empty,
        if (methodAccessors) Vector("methodAccessors") else Vector.empty,
        if (optionDefaultsToNone) Vector("optionDefaultsToNone") else Vector.empty,
        implicitConflictResolution.map(r => s"ImplicitTransformerPreference=$r").toList.toVector,
        if (displayMacrosLogging) Vector("displayMacrosLogging") else Vector.empty
      ).flatten.mkString(", ")})"
  }

  sealed abstract protected class RuntimeFieldOverride(val usesRuntimeDataStore: Boolean)
      extends Product
      with Serializable
  protected object RuntimeFieldOverride {
    final case class Const(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class ConstPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class Computed(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class ComputedPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class RenamedFrom(sourceName: String) extends RuntimeFieldOverride(false)
  }

  sealed abstract class RuntimeCoproductOverride extends Product with Serializable
  protected object RuntimeCoproductOverride {
    final case class CoproductInstance(runtimeDataIdx: Int) extends RuntimeCoproductOverride
    final case class CoproductInstancePartial(runtimeDataIdx: Int) extends RuntimeCoproductOverride
  }

  final protected case class TransformerConfig(
      flags: TransformerFlags = TransformerFlags(),
      fieldOverrides: Map[String, RuntimeFieldOverride] = Map.empty,
      coproductOverrides: Map[(ExistentialType, ExistentialType), RuntimeCoproductOverride] = Map.empty,
      preventResolutionForTypes: Option[(ExistentialType, ExistentialType)] = None
  ) {

    def prepareForRecursiveCall: TransformerConfig =
      copy(
        // preventResolutionForTypes = None,
        fieldOverrides = Map.empty
      )

    // def usesRuntimeDataStore: Boolean =
    //  fieldOverrides.values.exists(_.usesRuntimeDataStore) || coproductOverrides.nonEmpty

    def addFieldOverride(fieldName: String, fieldOverride: RuntimeFieldOverride): TransformerConfig =
      copy(fieldOverrides = fieldOverrides + (fieldName -> fieldOverride))

    def addCoproductInstance(
        instanceType: ExistentialType,
        targetType: ExistentialType,
        coproductOverride: RuntimeCoproductOverride
    ): TransformerConfig =
      copy(coproductOverrides = coproductOverrides + ((instanceType, targetType) -> coproductOverride))

    def withDefinitionScope(defScope: (ExistentialType, ExistentialType)): TransformerConfig =
      copy(preventResolutionForTypes = Some(defScope))

    override def toString: String = {
      val fieldOverridesString = fieldOverrides.map { case (k, v) => s"$k -> $v" }.mkString(", ")
      val coproductOverridesString = coproductOverrides
        .map { case ((f, t), v) => s"(${ExistentialType.prettyPrint(f)}, ${ExistentialType.prettyPrint(t)}) -> $v" }
        .mkString(", ")
      val preventResolutionForTypesString = preventResolutionForTypes.map { case (f, t) =>
        s"(${ExistentialType.prettyPrint(f)}, ${ExistentialType.prettyPrint(t)})"
      }.toString
      s"""TransformerConfig(
          |  flags = $flags,
          |  fieldOverrides = Map($fieldOverridesString),
          |  coproductOverrides = Map($coproductOverridesString),
          |  preventResolutionForTypes = $preventResolutionForTypesString
          |)""".stripMargin
    }
  }
  protected object TransformerConfig {

    type UpdateCfg[_ <: TransformerCfg]
  }

  protected val Configurations: ConfigurationsModule
  protected trait ConfigurationsModule { this: Configurations.type =>

    final def readTransformerConfig[
        Cfg <: internal.TransformerCfg: Type,
        InstanceFlags <: internal.TransformerFlags: Type,
        ImplicitScopeFlags <: internal.TransformerFlags: Type
    ]: TransformerConfig = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](TransformerFlags())
      val allFlags = extractTransformerFlags[InstanceFlags](implicitScopeFlags)
      extractTransformerConfig[Cfg](runtimeDataIdx = 0).copy(flags = allFlags)
    }

    type FlagsHead <: internal.TransformerFlags.Flag
    type FlagsTail <: internal.TransformerFlags
    private def extractTransformerFlags[Flag <: internal.TransformerFlags: Type](
        defaultFlags: TransformerFlags
    ): TransformerFlags = Type[Flag] match {
      case default if default =:= ChimneyType.TransformerFlags.Default => defaultFlags
      case ChimneyType.TransformerFlags.Enable(flag, flags) =>
        implicit val Flag: Type[FlagsHead] = flag.Underlying.asInstanceOf[Type[FlagsHead]]
        implicit val Flags: Type[FlagsTail] = flags.Underlying.asInstanceOf[Type[FlagsTail]]
        Flag match {
          case ChimneyType.TransformerFlags.Flags.ImplicitConflictResolution(r) =>
            if (r.Underlying =:= ChimneyType.PreferTotalTransformer)
              extractTransformerFlags[FlagsTail](defaultFlags).setImplicitConflictResolution(
                Some(dsls.PreferTotalTransformer)
              )
            else if (r.Underlying =:= ChimneyType.PreferPartialTransformer)
              extractTransformerFlags[FlagsTail](defaultFlags).setImplicitConflictResolution(
                Some(dsls.PreferPartialTransformer)
              )
            else {
              // $COVERAGE-OFF$
              reportError("Invalid implicit conflict resolution preference type!!")
              // $COVERAGE-ON$
            }
          case _ =>
            extractTransformerFlags[FlagsTail](defaultFlags).setBoolFlag[FlagsHead](value = true)
        }
      case ChimneyType.TransformerFlags.Disable(flag, flags) =>
        implicit val Flag: Type[FlagsHead] = flag.Underlying.asInstanceOf[Type[FlagsHead]]
        implicit val Flags: Type[FlagsTail] = flags.Underlying.asInstanceOf[Type[FlagsTail]]
        Flag match {
          case ChimneyType.TransformerFlags.Flags.ImplicitConflictResolution(_) =>
            extractTransformerFlags[FlagsTail](defaultFlags).setImplicitConflictResolution(None)
          case _ =>
            extractTransformerFlags[FlagsTail](defaultFlags).setBoolFlag[FlagsHead](value = false)
        }
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Bad internal transformer flags type shape ${Type.prettyPrint[Flag]}!")
      // $COVERAGE-ON$
    }

    // TODO: rewrite to shared
    protected def extractTransformerConfig[Cfg <: internal.TransformerCfg: Type](
        runtimeDataIdx: Int
    ): TransformerConfig
  }
}
