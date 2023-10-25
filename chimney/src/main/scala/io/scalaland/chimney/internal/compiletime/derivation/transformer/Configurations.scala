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
        reportError(s"Invalid internal TransformerFlag type: ${Type[Flag]}!")
        // $COVERAGE-ON$
      }

    def setImplicitConflictResolution(preference: Option[ImplicitTransformerPreference]): TransformerFlags =
      copy(implicitConflictResolution = preference)

    override def toString: String = s"TransformerFlags(${Vector(
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
    case object Root extends FieldPath {
      override def toString: String = "_"
    }
    final case class Select(name: String, instance: FieldPath) extends FieldPath {
      override def toString: String = s"$instance.$name"
    }

    object CurrentField {
      def unapply(path: FieldPath): Option[String] = path match {
        case Select(name, Root) => Some(name)
        case _                  => None
      }
    }
  }

  sealed abstract protected class RuntimeFieldOverride extends scala.Product with Serializable
  protected object RuntimeFieldOverride {
    final case class Const(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class ConstPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class Computed(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class ComputedPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class RenamedFrom(sourcePath: FieldPath, sourceValue: ExistentialExpr) extends RuntimeFieldOverride
  }

  sealed abstract protected class RuntimeCoproductOverride extends scala.Product with Serializable
  protected object RuntimeCoproductOverride {
    final case class CoproductInstance(runtimeDataIdx: Int) extends RuntimeCoproductOverride
    final case class CoproductInstancePartial(runtimeDataIdx: Int) extends RuntimeCoproductOverride
  }

  sealed abstract protected class FieldPathUpdate extends scala.Product with Serializable
  final protected case class DownField(nameFilter: String => Boolean) extends FieldPathUpdate
  protected object DownField { def apply(name: String): DownField = DownField(_ == name) }
  protected case object KeepFieldOverrides extends FieldPathUpdate
  protected case object CleanFieldOverrides extends FieldPathUpdate

  final protected case class OnRecur(
      fromField: FieldPathUpdate = CleanFieldOverrides,
      toField: FieldPathUpdate = CleanFieldOverrides
  )
  protected object OnRecur {
    val cleanAll: OnRecur = apply()
  }

  final protected case class TransformerConfig(
      flags: TransformerFlags = TransformerFlags(),
      private val instanceFlagOverridden: Boolean = false,
      private val fieldOverrides: Map[FieldPath, RuntimeFieldOverride] = Map.empty,
      private val coproductOverrides: Map[(??, ??), RuntimeCoproductOverride] = Map.empty,
      private val preventImplicitSummoningForTypes: Option[(??, ??)] = None
  ) {

    def prepareForRecursiveCall(tpe: OnRecur): TransformerConfig = {
      val newFieldOverrides: Map[FieldPath, RuntimeFieldOverride] = tpe.toField match {
        case DownField(nameFilter) =>
          fieldOverrides.view.flatMap {
            case (FieldPath.Select(toName, toPath), runtimeOverride) if (nameFilter(toName)) =>
              runtimeOverride match {
                case RuntimeFieldOverride.RenamedFrom(FieldPath.Select(fromName, fromPath), fromExpr)
                    if (nameFilter(fromName)) =>
                  Some(toPath -> RuntimeFieldOverride.RenamedFrom(fromPath, fromExpr))
                case _: RuntimeFieldOverride.RenamedFrom => None
                case _                                   => Some(toPath -> runtimeOverride)
              }
            case _ => None
          }.toMap
        case KeepFieldOverrides  => fieldOverrides
        case CleanFieldOverrides => Map.empty
      }
      copy(
        instanceFlagOverridden = false,
        fieldOverrides = newFieldOverrides,
        coproductOverrides = Map.empty,
        preventImplicitSummoningForTypes = None
      )
    }

    def allowFromToImplicitSearch: TransformerConfig = copy(preventImplicitSummoningForTypes = None)

    def setLocalFlagsOverriden: TransformerConfig = copy(instanceFlagOverridden = true)

    def addFieldOverride(fieldPath: FieldPath, fieldOverride: RuntimeFieldOverride): TransformerConfig =
      copy(fieldOverrides = fieldOverrides + (fieldPath -> fieldOverride))

    def addCoproductInstance[Instance: Type, Target: Type](
        coproductOverride: RuntimeCoproductOverride
    ): TransformerConfig =
      copy(coproductOverrides = coproductOverrides + ((Type[Instance].as_??, Type[Target].as_??) -> coproductOverride))

    def preventImplicitSummoningFor[From: Type, To: Type]: TransformerConfig =
      copy(preventImplicitSummoningForTypes = Some(Type[From].as_?? -> Type[To].as_??))

    def isImplicitSummoningPreventedFor[From: Type, To: Type]: Boolean =
      preventImplicitSummoningForTypes.exists { case (someFrom, someTo) =>
        import someFrom.Underlying as SomeFrom, someTo.Underlying as SomeTo
        Type[SomeFrom] =:= Type[From] && Type[SomeTo] =:= Type[To]
      }

    def areOverridesEmptyForCurrent[From: Type, To: Type]: Boolean =
      !instanceFlagOverridden && fieldOverrides.isEmpty && filterOverridesForCoproduct { (someFrom, someTo) =>
        import someFrom.Underlying as SomeFrom, someTo.Underlying as SomeTo
        Type[SomeFrom] <:< Type[From] && Type[To] <:< Type[SomeTo]
      }.isEmpty

    def filterOverridesForField(nameFilter: String => Boolean): Map[String, RuntimeFieldOverride] =
      fieldOverrides.view.collect {
        case (FieldPath.CurrentField(fieldName), fieldOverride) if nameFilter(fieldName) =>
          fieldName -> fieldOverride
      }.toMap

    def filterOverridesForCoproduct(typeFilter: (??, ??) => Boolean): Map[(??, ??), RuntimeCoproductOverride] =
      coproductOverrides.view.filter(p => typeFilter.tupled(p._1)).toMap

    override def toString: String = {
      val fieldOverridesString = fieldOverrides.map { case (k, v) => s"$k -> $v" }.mkString(", ")
      val coproductOverridesString = coproductOverrides
        .map { case ((f, t), v) => s"(${ExistentialType.prettyPrint(f)}, ${ExistentialType.prettyPrint(t)}) -> $v" }
        .mkString(", ")
      val preventImplicitSummoningForTypesString = preventImplicitSummoningForTypes.map { case (f, t) =>
        s"(${ExistentialType.prettyPrint(f)}, ${ExistentialType.prettyPrint(t)})"
      }.toString
      s"""TransformerConfig(
         |  flags = $flags,
         |  instanceFlagOverridden = $instanceFlagOverridden,
         |  fieldOverrides = Map($fieldOverridesString),
         |  coproductOverrides = Map($coproductOverridesString),
         |  preventImplicitSummoningForTypes = $preventImplicitSummoningForTypesString
         |)""".stripMargin
    }
  }

  protected object TransformerConfigurations {

    final def readTransformerConfig[
        Cfg <: runtime.TransformerCfg: Type,
        InstanceFlags <: runtime.TransformerFlags: Type,
        ImplicitScopeFlags <: runtime.TransformerFlags: Type
    ](fromExpr: ExistentialExpr): TransformerConfig = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](TransformerFlags())
      val allFlags = extractTransformerFlags[InstanceFlags](implicitScopeFlags)
      val cfg = extractTransformerConfig[Cfg](runtimeDataIdx = 0, fromExpr = fromExpr).copy(flags = allFlags)
      if (Type[InstanceFlags] =:= ChimneyType.TransformerFlags.Default) cfg else cfg.setLocalFlagsOverriden
    }

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
              reportError("Invalid ImplicitTransformerPreference type!!")
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
        reportError(s"Invalid internal TransformerFlags type shape: ${Type.prettyPrint[Flags]}!")
      // $COVERAGE-ON$
    }

    private def extractTransformerConfig[Cfg <: runtime.TransformerCfg: Type](
        runtimeDataIdx: Int,
        fromExpr: ExistentialExpr
    ): TransformerConfig = Type[Cfg] match {
      case empty if empty =:= ChimneyType.TransformerCfg.Empty => TransformerConfig()
      case ChimneyType.TransformerCfg.FieldConst(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx, fromExpr)
          .addFieldOverride(
            extractPath[FieldName],
            RuntimeFieldOverride.Const(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldConstPartial(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx, fromExpr)
          .addFieldOverride(
            extractPath[FieldName],
            RuntimeFieldOverride.ConstPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldComputed(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx, fromExpr)
          .addFieldOverride(
            extractPath[FieldName],
            RuntimeFieldOverride.Computed(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldComputedPartial(fieldName, cfg) =>
        import fieldName.Underlying as FieldName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx, fromExpr)
          .addFieldOverride(
            extractPath[FieldName],
            RuntimeFieldOverride.ComputedPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldRelabelled(fromName, toName, cfg) =>
        import fromName.Underlying as FromName, toName.Underlying as ToName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](runtimeDataIdx, fromExpr)
          .addFieldOverride(
            extractPath[ToName],
            RuntimeFieldOverride.RenamedFrom(extractPath[FromName], fromExpr)
          )
      case ChimneyType.TransformerCfg.CoproductInstance(instance, target, cfg) =>
        import instance.Underlying as Instance, target.Underlying as Target, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx, fromExpr)
          .addCoproductInstance[Instance, Target](
            RuntimeCoproductOverride.CoproductInstance(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.CoproductInstancePartial(instance, target, cfg) =>
        import instance.Underlying as Instance, target.Underlying as Target, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx, fromExpr)
          .addCoproductInstance[Instance, Target](
            RuntimeCoproductOverride.CoproductInstancePartial(runtimeDataIdx)
          )
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Invalid internal TransformerCfg type shape: ${Type.prettyPrint[Cfg]}!!")
      // $COVERAGE-ON$
    }

    private def extractPath[Field <: runtime.Path: Type]: FieldPath = Type[Field] match {
      case root if root =:= ChimneyType.Path.Root =>
        FieldPath.Root
      case ChimneyType.Path.Select(fieldName, path) =>
        import fieldName.Underlying as FieldName, path.Underlying as Path
        FieldPath.Select(Type[FieldName].extractStringSingleton, extractPath[Path])
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Invalid internal Path shape: ${Type.prettyPrint[Field]}!!")
      // $COVERAGE-ON$
    }
  }
}
