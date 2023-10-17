package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.ImplicitTransformerPreference
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime

private[compiletime] trait Configurations { this: Derivation =>

  final protected case class TransformerFlags(
      inheritedAccessors: Boolean = false,
      methodAccessors: Boolean = false,
      processDefaultValues: Boolean = false,
      beanSetters: Boolean = false,
      beanGetters: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      implicitConflictResolution: Option[ImplicitTransformerPreference] = None,
      displayMacrosLogging: Boolean = false
  ) {

    def setBoolFlag[Flag <: runtime.TransformerFlags.Flag: Type](value: Boolean): TransformerFlags =
      if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.InheritedAccessors) {
        copy(inheritedAccessors = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.MethodAccessors) {
        copy(methodAccessors = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.DefaultValues) {
        copy(processDefaultValues = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.BeanSetters) {
        copy(beanSetters = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.BeanGetters) {
        copy(beanGetters = value)
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
        if (inheritedAccessors) Vector("inheritedAccessors") else Vector.empty,
        if (methodAccessors) Vector("methodAccessors") else Vector.empty,
        if (processDefaultValues) Vector("processDefaultValues") else Vector.empty,
        if (beanSetters) Vector("beanSetters") else Vector.empty,
        if (beanGetters) Vector("beanGetters") else Vector.empty,
        if (optionDefaultsToNone) Vector("optionDefaultsToNone") else Vector.empty,
        implicitConflictResolution.map(r => s"ImplicitTransformerPreference=$r").toList.toVector,
        if (displayMacrosLogging) Vector("displayMacrosLogging") else Vector.empty
      ).flatten.mkString(", ")})"
  }

  sealed abstract protected class FieldPath extends scala.Product with Serializable
  protected object FieldPath {
    case object Root extends FieldPath
    final case class Select(name: String, instance: FieldPath) extends FieldPath
  }

  sealed abstract protected class RuntimeFieldOverride extends scala.Product with Serializable
  protected object RuntimeFieldOverride {
    final case class Const(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class ConstPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class Computed(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class ComputedPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class RenamedFrom(sourceName: String) extends RuntimeFieldOverride
  }

  sealed abstract class RuntimeCoproductOverride extends scala.Product with Serializable
  protected object RuntimeCoproductOverride {
    final case class CoproductInstance(runtimeDataIdx: Int) extends RuntimeCoproductOverride
    final case class CoproductInstancePartial(runtimeDataIdx: Int) extends RuntimeCoproductOverride
  }

  final protected case class TransformerConfig(
      flags: TransformerFlags = TransformerFlags(),
      private val fieldOverrides: Map[String, RuntimeFieldOverride] = Map.empty, // TODO: use Path instead of String
      private val coproductOverrides: Map[(??, ??), RuntimeCoproductOverride] = Map.empty,
      private val preventResolutionForTypes: Option[(??, ??)] = None
  ) {

    // When going recursively we have to:
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
    def prepareForRecursiveCall: TransformerConfig = copy(preventResolutionForTypes = None)

    // TODO: descend at name for each fieldOverride
    def prepareForRecursiveCallAtField(nameFilter: String => Boolean): TransformerConfig =
      // When going recursively for field we have to:
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
      prepareForRecursiveCall.copy(
        // TODO: update for FieldPath when available
        fieldOverrides = fieldOverrides.view.flatMap {
          case (fieldName, runtimeOverride) if (nameFilter(fieldName)) => Some(fieldName -> runtimeOverride)
          case _                                                       => None
        }.toMap
      )

    def allowFromToImplicitSearch: TransformerConfig = copy(preventResolutionForTypes = None)

    // TODO use Path instead of String
    def addFieldOverride(fieldName: String, fieldOverride: RuntimeFieldOverride): TransformerConfig =
      copy(fieldOverrides = fieldOverrides + (fieldName -> fieldOverride))

    def addCoproductInstance(
        instanceType: ??,
        targetType: ??,
        coproductOverride: RuntimeCoproductOverride
    ): TransformerConfig =
      copy(coproductOverrides = coproductOverrides + ((instanceType, targetType) -> coproductOverride))

    def preventResolutionFor[From: Type, To: Type]: TransformerConfig =
      copy(preventResolutionForTypes = Some(Type[From].as_?? -> Type[To].as_??))

    // prevents: val t: Transformer[A, B] = a => t.transform(a)
    def isResolutionPreventedFor[From: Type, To: Type]: Boolean =
      preventResolutionForTypes.exists { case (someFrom, someTo) =>
        import someFrom.Underlying as SomeFrom, someTo.Underlying as SomeTo
        Type[SomeFrom] =:= Type[From] && Type[SomeTo] =:= Type[To]
      }

    def areOverridesEmptyForCurrent[From: Type, To: Type]: Boolean =
      fieldOverrides.isEmpty && filterOverridesForCoproduct { (someFrom, someTo) =>
        import someFrom.Underlying as SomeFrom, someTo.Underlying as SomeTo
        Type[SomeFrom] <:< Type[From] && Type[To] <:< Type[SomeTo]
      }.isEmpty

    def filterOverridesForField(nameFilter: String => Boolean): Map[String, RuntimeFieldOverride] =
      fieldOverrides.view.filterKeys(nameFilter).toMap

    def filterOverridesForCoproduct(typeFilter: (??, ??) => Boolean): Map[(??, ??), RuntimeCoproductOverride] =
      coproductOverrides.view.filterKeys(typeFilter.tupled).toMap

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
        import flag.Underlying as Flag, flags.Underlying as Flags2
        Flag match {
          case ChimneyType.TransformerFlags.Flags.ImplicitConflictResolution(r) =>
            if (r.Underlying =:= ChimneyType.PreferTotalTransformer)
              extractTransformerFlags[Flags2](defaultFlags).setImplicitConflictResolution(
                Some(dsls.PreferTotalTransformer)
              )
            else if (r.Underlying =:= ChimneyType.PreferPartialTransformer)
              extractTransformerFlags[Flags2](defaultFlags).setImplicitConflictResolution(
                Some(dsls.PreferPartialTransformer)
              )
            else {
              // $COVERAGE-OFF$
              reportError("Invalid implicit conflict resolution preference type!!")
              // $COVERAGE-ON$
            }
          case _ =>
            extractTransformerFlags[Flags2](defaultFlags).setBoolFlag[Flag](value = true)
        }
      case ChimneyType.TransformerFlags.Disable(flag, flags) =>
        import flag.Underlying as Flag, flags.Underlying as Flags2
        Flag match {
          case ChimneyType.TransformerFlags.Flags.ImplicitConflictResolution(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setImplicitConflictResolution(None)
          case _ =>
            extractTransformerFlags[Flags2](defaultFlags).setBoolFlag[Flag](value = false)
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
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addFieldOverride(
            extractPath[FieldName],
            RuntimeFieldOverride.Const(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldConstPartial(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addFieldOverride(
            extractPath[FieldName],
            RuntimeFieldOverride.ConstPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldComputed(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addFieldOverride(
            extractPath[FieldName],
            RuntimeFieldOverride.Computed(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldComputedPartial(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addFieldOverride(
            extractPath[FieldName],
            RuntimeFieldOverride.ComputedPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldRelabelled(fromName, toName, cfg) =>
        import fromName.Underlying as FromName, toName.Underlying as ToName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](runtimeDataIdx)
          .addFieldOverride(
            extractPath[ToName],
            RuntimeFieldOverride.RenamedFrom(extractPath[FromName])
          )
      case ChimneyType.TransformerCfg.CoproductInstance(instance, target, cfg) =>
        import instance.Underlying as Instance, target.Underlying as Target, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addCoproductInstance(
            Type[Instance].as_??,
            Type[Target].as_??,
            RuntimeCoproductOverride.CoproductInstance(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.CoproductInstancePartial(instance, target, cfg) =>
        import instance.Underlying as Instance, target.Underlying as Target, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addCoproductInstance(
            Type[Instance].as_??,
            Type[Target].as_??,
            RuntimeCoproductOverride.CoproductInstancePartial(runtimeDataIdx)
          )
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Bad internal transformer config type shape ${Type.prettyPrint[Cfg]}!!")
      // $COVERAGE-ON$
    }

    // currently we aren't supporting nested paths
    // This (suppressed) error is a case when compiler is simply wrong :)
    @scala.annotation.nowarn("msg=Unreachable case")
    private def extractPath[Field <: runtime.Path: Type]: String = Type[Field] match {
      case ChimneyType.Path.Select(fieldName, path) if path.value =:= ChimneyType.Path.Root =>
        import fieldName.Underlying as FieldName, path.Underlying as Path
        Type[Path] match {
          case root if root =:= ChimneyType.Path.Root => Type[FieldName].extractStringSingleton
          case _                                      =>
            // $COVERAGE-OFF$
            reportError(s"Nested paths ${Type.prettyPrint[Field]} are not supported!!")
          // $COVERAGE-ON$
        }
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Bad paths shape ${Type.prettyPrint[Field]}!!")
      // $COVERAGE-ON$
    }
  }
}
