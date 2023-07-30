package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.ImplicitTransformerPreference
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime

private[compiletime] trait Configurations { this: Derivation =>

  final protected case class TransformerFlags(
      processDefaultValues: Boolean = false,
      beanSetters: Boolean = false,
      beanGetters: Boolean = false,
      methodAccessors: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      implicitConflictResolution: Option[ImplicitTransformerPreference] = None,
      displayMacrosLogging: Boolean = false
  ) {

    def setBoolFlag[Flag <: runtime.TransformerFlags.Flag: Type](value: Boolean): TransformerFlags =
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
      extends scala.Product
      with Serializable
  protected object RuntimeFieldOverride {
    final case class Const(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class ConstPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class Computed(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class ComputedPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride(true)
    final case class RenamedFrom(sourceName: String) extends RuntimeFieldOverride(false)
  }

  sealed abstract class RuntimeCoproductOverride extends scala.Product with Serializable
  protected object RuntimeCoproductOverride {
    final case class CoproductInstance(runtimeDataIdx: Int) extends RuntimeCoproductOverride
    final case class CoproductInstancePartial(runtimeDataIdx: Int) extends RuntimeCoproductOverride
  }

  final protected case class TransformerConfig(
      flags: TransformerFlags = TransformerFlags(),
      fieldOverrides: Map[String, RuntimeFieldOverride] = Map.empty,
      coproductOverrides: Map[(??, ??), RuntimeCoproductOverride] = Map.empty,
      preventResolutionForTypes: Option[(??, ??)] = None
  ) {

    def allowFromToImplicitSearch: TransformerConfig = copy(preventResolutionForTypes = None)

    def prepareForRecursiveCall: TransformerConfig =
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

    def addFieldOverride(fieldName: String, fieldOverride: RuntimeFieldOverride): TransformerConfig =
      copy(fieldOverrides = fieldOverrides + (fieldName -> fieldOverride))

    def addCoproductInstance(
        instanceType: ??,
        targetType: ??,
        coproductOverride: RuntimeCoproductOverride
    ): TransformerConfig =
      copy(coproductOverrides = coproductOverrides + ((instanceType, targetType) -> coproductOverride))

    def withDefinitionScope(defScope: (??, ??)): TransformerConfig =
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

  protected object TransformerConfigurations {

    final def readTransformerConfig[
        Cfg <: runtime.TransformerCfg: Type,
        InstanceFlags <: runtime.TransformerFlags: Type,
        ImplicitScopeFlags <: runtime.TransformerFlags: Type
    ]: TransformerConfig = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](TransformerFlags())
      val allFlags = extractTransformerFlags[InstanceFlags](implicitScopeFlags)
      extractTransformerConfig[Cfg](runtimeDataIdx = 0).copy(flags = allFlags)
    }

    // This (suppressed) error is a case when compiler is simply wrong :)
    @scala.annotation.nowarn("msg=Unreachable case")
    private def extractTransformerFlags[Flags <: runtime.TransformerFlags: Type](
        defaultFlags: TransformerFlags
    ): TransformerFlags = Type[Flags] match {
      case default if default =:= ChimneyType.TransformerFlags.Default => defaultFlags
      case ChimneyType.TransformerFlags.Enable(flag, flags) =>
        import flag.Underlying as Flag, flags.Underlying as Flags
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
      case ChimneyType.TransformerFlags.Disable(flag, flags) =>
        import flag.Underlying as Flag, flags.Underlying as Flags
        Flag match {
          case ChimneyType.TransformerFlags.Flags.ImplicitConflictResolution(_) =>
            extractTransformerFlags[flags.Underlying](defaultFlags).setImplicitConflictResolution(None)
          case _ =>
            extractTransformerFlags[flags.Underlying](defaultFlags).setBoolFlag[flag.Underlying](value = false)
        }
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Bad internal transformer flags type shape ${Type.prettyPrint[Flags]}!")
      // $COVERAGE-ON$
    }

    // This (suppressed) error is a case when compiler is simply wrong :)
    @scala.annotation.nowarn("msg=Unreachable case")
    private def extractTransformerConfig[Cfg <: runtime.TransformerCfg: Type](
        runtimeDataIdx: Int
    ): TransformerConfig = Type[Cfg] match {
      case empty if empty =:= ChimneyType.TransformerCfg.Empty => TransformerConfig()
      case ChimneyType.TransformerCfg.FieldConst(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg
        extractTransformerConfig[cfg.Underlying](1 + runtimeDataIdx)
          .addFieldOverride(
            Type[fieldName.Underlying].extractStringSingleton,
            RuntimeFieldOverride.Const(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldConstPartial(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg
        extractTransformerConfig[cfg.Underlying](1 + runtimeDataIdx)
          .addFieldOverride(
            Type[fieldName.Underlying].extractStringSingleton,
            RuntimeFieldOverride.ConstPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldComputed(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg
        extractTransformerConfig[cfg.Underlying](1 + runtimeDataIdx)
          .addFieldOverride(
            Type[fieldName.Underlying].extractStringSingleton,
            RuntimeFieldOverride.Computed(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldComputedPartial(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg
        extractTransformerConfig[cfg.Underlying](1 + runtimeDataIdx)
          .addFieldOverride(
            Type[fieldName.Underlying].extractStringSingleton,
            RuntimeFieldOverride.ComputedPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldRelabelled(fromName, toName, cfg) =>
        import fromName.Underlying as FromName, toName.Underlying as ToName, cfg.Underlying as Cfg
        extractTransformerConfig[cfg.Underlying](runtimeDataIdx)
          .addFieldOverride(
            Type[toName.Underlying].extractStringSingleton,
            RuntimeFieldOverride.RenamedFrom(Type[fromName.Underlying].extractStringSingleton)
          )
      case ChimneyType.TransformerCfg.CoproductInstance(instance, target, cfg) =>
        import instance.Underlying as Instance, target.Underlying as Target, cfg.Underlying as Cfg
        extractTransformerConfig[cfg.Underlying](1 + runtimeDataIdx)
          .addCoproductInstance(
            Type[instance.Underlying].as_??,
            Type[target.Underlying].as_??,
            RuntimeCoproductOverride.CoproductInstance(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.CoproductInstancePartial(instance, target, cfg) =>
        import instance.Underlying as Instance, target.Underlying as Target, cfg.Underlying as Cfg
        extractTransformerConfig[cfg.Underlying](1 + runtimeDataIdx)
          .addCoproductInstance(
            Type[instance.Underlying].as_??,
            Type[target.Underlying].as_??,
            RuntimeCoproductOverride.CoproductInstancePartial(runtimeDataIdx)
          )
      case _ =>
        reportError(s"Bad internal transformer config type shape ${Type.prettyPrint[Cfg]}!!")
    }
  }
}
