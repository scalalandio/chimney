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

    def allowFromToImplicitSearch: TransformerConfig = copy(preventResolutionForTypes = None)

    def prepareForRecursiveCall: TransformerConfig = {
      // When going recursively we have to:
      // - clear the field overrides since `with*(_.field, *)` might make sense for src, but not for src.field
      // - clear implicit call prevention:
      //   - we want to prevent
      //   {{{
      //   implicit val foobar: Transformer[Foo, Bar] = foo => summon[Transformer[Foo, Bar]].transform(foo)
      //   }}}
      // - but we don't want to prevent:
      //   {{{
      //   implicit val foobar: Transformer[Foo, Bar] = foo => Bar(
      //     foo.x.map(foo2 => summon[Transformer[Foo, Bar]].transform(foo2))
      //   )
      //   }}}
      copy(
        preventResolutionForTypes = None,
        fieldOverrides = Map.empty
      )
    }

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

    // This (suppressed) error is a case when compiler is simply wrong :)
    @scala.annotation.nowarn("msg=Unreachable case")
    private def extractTransformerFlags[Flags <: internal.TransformerFlags: Type](
        defaultFlags: TransformerFlags
    ): TransformerFlags = Type[Flags] match {
      case default if default =:= ChimneyType.TransformerFlags.Default => defaultFlags
      case ChimneyType.TransformerFlags.Enable(flag, flags) =>
        ExistentialType.Bounded.use2(flag, flags) {
          implicit Flag: Type[flag.Underlying] => implicit Flags: Type[flags.Underlying] =>
            Flag match {
              case ChimneyType.TransformerFlags.Flags.ImplicitConflictResolution(r) =>
                if (r.Underlying =:= ChimneyType.PreferTotalTransformer)
                  extractTransformerFlags[flags.Underlying](defaultFlags).setImplicitConflictResolution(
                    Some(dsls.PreferTotalTransformer)
                  )
                else if (r.Underlying =:= ChimneyType.PreferPartialTransformer)
                  extractTransformerFlags[flags.Underlying](defaultFlags).setImplicitConflictResolution(
                    Some(dsls.PreferPartialTransformer)
                  )
                else {
                  // $COVERAGE-OFF$
                  reportError("Invalid implicit conflict resolution preference type!!")
                  // $COVERAGE-ON$
                }
              case _ =>
                extractTransformerFlags[flags.Underlying](defaultFlags).setBoolFlag[flag.Underlying](value = true)
            }
        }
      case ChimneyType.TransformerFlags.Disable(flag, flags) =>
        ExistentialType.Bounded.use2(flag, flags) {
          implicit Flag: Type[flag.Underlying] => implicit Flags: Type[flags.Underlying] =>
            Flag match {
              case ChimneyType.TransformerFlags.Flags.ImplicitConflictResolution(_) =>
                extractTransformerFlags[flags.Underlying](defaultFlags).setImplicitConflictResolution(None)
              case _ =>
                extractTransformerFlags[flags.Underlying](defaultFlags).setBoolFlag[flag.Underlying](value = false)
            }
        }
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Bad internal transformer flags type shape ${Type.prettyPrint[Flags]}!")
      // $COVERAGE-ON$
    }

    // TODO: rewrite to shared
    protected def extractTransformerConfig[Cfg <: internal.TransformerCfg: Type](
        runtimeDataIdx: Int
    ): TransformerConfig
  }
}
