package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.ImplicitTransformerPreference
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime

import scala.collection.compat.*
import scala.collection.immutable.ListMap

private[compiletime] trait Configurations { this: Derivation =>

  final protected case class TransformerFlags(
      inheritedAccessors: Boolean = false,
      methodAccessors: Boolean = false,
      processDefaultValues: Boolean = false,
      beanSetters: Boolean = false,
      beanSettersIgnoreUnmatched: Boolean = false,
      beanGetters: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      partialUnwrapsOption: Boolean = true,
      implicitConflictResolution: Option[ImplicitTransformerPreference] = None,
      fieldNameComparison: Option[dsls.TransformedNamesComparison] = None,
      subtypeNameComparison: Option[dsls.TransformedNamesComparison] = None,
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
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.BeanSettersIgnoreUnmatched) {
        copy(beanSettersIgnoreUnmatched = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.BeanGetters) {
        copy(beanGetters = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.OptionDefaultsToNone) {
        copy(optionDefaultsToNone = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.PartialUnwrapsOption) {
        copy(partialUnwrapsOption = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.MacrosLogging) {
        copy(displayMacrosLogging = value)
      } else {
        // $COVERAGE-OFF$
        reportError(s"Invalid internal TransformerFlag type: ${Type[Flag]}!")
        // $COVERAGE-ON$
      }

    def setImplicitConflictResolution(preference: Option[ImplicitTransformerPreference]): TransformerFlags =
      copy(implicitConflictResolution = preference)

    def setFieldNameComparison(nameComparison: Option[dsls.TransformedNamesComparison]): TransformerFlags =
      copy(fieldNameComparison = nameComparison)

    def setSubtypeNameComparison(nameComparison: Option[dsls.TransformedNamesComparison]): TransformerFlags =
      copy(subtypeNameComparison = nameComparison)

    override def toString: String = s"TransformerFlags(${Vector(
        if (inheritedAccessors) Vector("inheritedAccessors") else Vector.empty,
        if (methodAccessors) Vector("methodAccessors") else Vector.empty,
        if (processDefaultValues) Vector("processDefaultValues") else Vector.empty,
        if (beanSetters) Vector("beanSetters") else Vector.empty,
        if (beanGetters) Vector("beanGetters") else Vector.empty,
        if (optionDefaultsToNone) Vector("optionDefaultsToNone") else Vector.empty,
        implicitConflictResolution.map(r => s"ImplicitTransformerPreference=$r").toList.toVector,
        fieldNameComparison.map(r => s"fieldNameComparison=$r").toList.toVector,
        subtypeNameComparison.map(r => s"subtypeNameComparison=$r").toList.toVector,
        if (displayMacrosLogging) Vector("displayMacrosLogging") else Vector.empty
      ).flatten.mkString(", ")})"
  }

  protected type FieldPath = Vector[String]
  protected object FieldPath {
    val Root: FieldPath = Vector()
    object Select {
      def apply(name: String, path: FieldPath): FieldPath = path :+ name
      def unapply(path: FieldPath): Option[(String, FieldPath)] = path.lastOption.map(_ -> path.take(path.size - 1))
    }
    object Prepended {
      def unapply(path: FieldPath): Option[(String, FieldPath)] = path.headOption.map(_ -> path.drop(1))
    }
    object CurrentField {
      def unapply(path: FieldPath): Option[String] = if (path.size == 1) path.headOption else None
    }
    def print(path: FieldPath): String = "_" + path.map(s => "." + s).mkString
  }

  sealed abstract protected class RuntimeFieldOverride extends scala.Product with Serializable
  protected object RuntimeFieldOverride {
    final case class Const(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class ConstPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class Computed(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class ComputedPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class RenamedFrom(sourcePath: FieldPath) extends RuntimeFieldOverride {
      override def toString: String = s"RenamedFrom(${FieldPath.print(sourcePath)})"
    }
  }

  sealed abstract protected class RuntimeCoproductOverride extends scala.Product with Serializable
  protected object RuntimeCoproductOverride {
    final case class CoproductInstance(runtimeDataIdx: Int) extends RuntimeCoproductOverride
    final case class CoproductInstancePartial(runtimeDataIdx: Int) extends RuntimeCoproductOverride
  }

  sealed abstract protected class RuntimeConstructorOverride extends scala.Product with Serializable
  protected object RuntimeConstructorOverride {
    type Args = List[ListMap[String, ??]]

    final case class Constructor(runtimeDataIdx: Int, args: Args) extends RuntimeConstructorOverride {
      override def toString: String = s"Constructor($runtimeDataIdx, ${printArgs(args)})"
    }
    final case class ConstructorPartial(runtimeDataIdx: Int, args: Args) extends RuntimeConstructorOverride {
      override def toString: String = s"ConstructorPartial($runtimeDataIdx, ${printArgs(args)})"
    }

    import ExistentialType.prettyPrint
    private def printArgs(args: Args): String =
      if (args.isEmpty) "<no list>"
      else args.map(list => "(" + list.map { case (n, t) => s"$n: ${prettyPrint(t)}" }.mkString(", ") + ")").mkString
  }

  sealed abstract protected class FieldPathUpdate extends scala.Product with Serializable {

    final lazy val update: FieldPath => Option[FieldPath] = this match {
      case DownField(nameFilter) => {
        case FieldPath.Prepended(fieldName, remainingPath) if (nameFilter(fieldName)) => Some(remainingPath)
        case _                                                                        => None
      }
      case KeepFieldOverrides  => path => Some(path)
      case CleanFieldOverrides => _ => None
    }
  }
  final protected case class DownField(nameFilter: String => Boolean) extends FieldPathUpdate
  protected case object KeepFieldOverrides extends FieldPathUpdate
  protected case object CleanFieldOverrides extends FieldPathUpdate

  final protected case class TransformerConfig(
      flags: TransformerFlags = TransformerFlags(),
      private val instanceFlagOverridden: Boolean = false,
      private val fieldOverrides: Map[FieldPath, RuntimeFieldOverride] = Map.empty,
      private val coproductOverrides: Map[(??, ??), RuntimeCoproductOverride] = Map.empty,
      private val constructorOverrides: Map[??, RuntimeConstructorOverride] = Map.empty,
      private val preventImplicitSummoningForTypes: Option[(??, ??)] = None
  ) {

    def prepareForRecursiveCall(updateTo: FieldPathUpdate): TransformerConfig = {
      val newFieldOverrides: Map[FieldPath, RuntimeFieldOverride] = fieldOverrides.view.flatMap {
        case (toPath, runtimeOverride) =>
          // path to field is expected to be _.fieldName when matching, _ is unexpected
          updateTo.update(toPath).filter(_ != FieldPath.Root).map(_ -> runtimeOverride).toMap
      }.toMap

      copy(
        instanceFlagOverridden = false,
        fieldOverrides = newFieldOverrides,
        coproductOverrides = Map.empty,
        constructorOverrides = Map.empty,
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

    def addConstructor[Target: Type](constructorOverride: RuntimeConstructorOverride): TransformerConfig =
      copy(constructorOverrides = constructorOverrides + ((Type[Target].as_?? -> constructorOverride)))

    def preventImplicitSummoningFor[From: Type, To: Type]: TransformerConfig =
      copy(preventImplicitSummoningForTypes = Some(Type[From].as_?? -> Type[To].as_??))

    def isImplicitSummoningPreventedFor[From: Type, To: Type]: Boolean =
      preventImplicitSummoningForTypes.exists { case (someFrom, someTo) =>
        import someFrom.Underlying as SomeFrom, someTo.Underlying as SomeTo
        Type[SomeFrom] =:= Type[From] && Type[SomeTo] =:= Type[To]
      }

    def areFlagOverridesEmptyForCurrent[From: Type, To: Type]: Boolean =
      !instanceFlagOverridden

    def areValueOverridesEmptyForCurrent[From: Type, To: Type]: Boolean =
      fieldOverrides.isEmpty && filterOverridesForCoproduct { (someFrom, someTo) =>
        import someFrom.Underlying as SomeFrom, someTo.Underlying as SomeTo
        Type[SomeFrom] <:< Type[From] && Type[To] <:< Type[SomeTo]
      }.isEmpty && filterOverridesForConstructor { someTo =>
        import someTo.Underlying as SomeTo
        Type[To] <:< Type[SomeTo]
      }.isEmpty

    def areOverridesEmptyForCurrent[From: Type, To: Type]: Boolean =
      areFlagOverridesEmptyForCurrent[From, To] && areValueOverridesEmptyForCurrent[From, To]

    def filterOverridesForField(nameFilter: String => Boolean): Map[String, RuntimeFieldOverride] =
      fieldOverrides.view.collect {
        case (FieldPath.CurrentField(fieldName), fieldOverride) if nameFilter(fieldName) =>
          fieldName -> fieldOverride
      }.toMap

    def filterOverridesForCoproduct(typeFilter: (??, ??) => Boolean): Map[(??, ??), RuntimeCoproductOverride] =
      coproductOverrides.view.filter(p => typeFilter.tupled(p._1)).toMap

    def filterOverridesForConstructor(typeFilter: ?? => Boolean): Map[??, RuntimeConstructorOverride] =
      constructorOverrides.view.filter(p => typeFilter(p._1)).toMap

    override def toString: String = {
      val fieldOverridesString = fieldOverrides.map { case (k, v) => s"${FieldPath.print(k)} -> $v" }.mkString(", ")
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
    ]: TransformerConfig = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](TransformerFlags())
      val allFlags = extractTransformerFlags[InstanceFlags](implicitScopeFlags)
      val cfg = extractTransformerConfig[Cfg](runtimeDataIdx = 0).copy(flags = allFlags)
      if (Type[InstanceFlags] =:= ChimneyType.TransformerFlags.Default) cfg else cfg.setLocalFlagsOverriden
    }

    private def extractArgumentList[Args <: runtime.ArgumentList: Type]: List[(String, ??)] =
      Type[Args] match {
        case empty if empty =:= ChimneyType.ArgumentList.Empty => List.empty
        case ChimneyType.ArgumentList.Argument(name, tpe, args) =>
          import name.Underlying as Name, args.Underlying as Args2
          (Type[Name].extractStringSingleton, tpe) :: extractArgumentList[Args2]
      }

    private def extractArgumentLists[Args <: runtime.ArgumentLists: Type]: List[ListMap[String, ??]] =
      Type[Args] match {
        case empty if empty =:= ChimneyType.ArgumentLists.Empty => List.empty
        case ChimneyType.ArgumentLists.List(head, tail) =>
          import head.Underlying as Head, tail.Underlying as Tail
          ListMap.from(extractArgumentList[Head]) :: extractArgumentLists[Tail]
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
          case ChimneyType.TransformerFlags.Flags.FieldNameComparison(c) =>
            import c.Underlying as Comparison
            extractTransformerFlags[Flags2](defaultFlags).setFieldNameComparison(
              Some(extractNameComparisonObject[Comparison])
            )
          case ChimneyType.TransformerFlags.Flags.SubtypeNameComparison(c) =>
            import c.Underlying as Comparison
            extractTransformerFlags[Flags2](defaultFlags).setSubtypeNameComparison(
              Some(extractNameComparisonObject[Comparison])
            )
          case _ =>
            extractTransformerFlags[Flags2](defaultFlags).setBoolFlag[Flag](value = true)
        }
      case ChimneyType.TransformerFlags.Disable(flag, flags) =>
        import flag.Underlying as Flag, flags.Underlying as Flags2
        Flag match {
          case ChimneyType.TransformerFlags.Flags.ImplicitConflictResolution(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setImplicitConflictResolution(None)
          case ChimneyType.TransformerFlags.Flags.FieldNameComparison(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setFieldNameComparison(None)
          case ChimneyType.TransformerFlags.Flags.SubtypeNameComparison(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setSubtypeNameComparison(None)
          case _ =>
            extractTransformerFlags[Flags2](defaultFlags).setBoolFlag[Flag](value = false)
        }
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Invalid internal TransformerFlags type shape: ${Type.prettyPrint[Flags]}!")
      // $COVERAGE-ON$
    }

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
          .addCoproductInstance[Instance, Target](
            RuntimeCoproductOverride.CoproductInstance(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.CoproductInstancePartial(instance, target, cfg) =>
        import instance.Underlying as Instance, target.Underlying as Target, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addCoproductInstance[Instance, Target](
            RuntimeCoproductOverride.CoproductInstancePartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.Constructor(args, target, cfg) =>
        import args.Underlying as Args, target.Underlying as Target, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addConstructor[Target](
            RuntimeConstructorOverride.Constructor(runtimeDataIdx, extractArgumentLists[Args])
          )
      case ChimneyType.TransformerCfg.ConstructorPartial(args, target, cfg) =>
        import args.Underlying as Args, target.Underlying as Target, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addConstructor[Target](
            RuntimeConstructorOverride.ConstructorPartial(runtimeDataIdx, extractArgumentLists[Args])
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

    // TODO: consider moving this utils to Type and requiring <: Singleton type-bound
    private val AnsiControlCode = "\u001b\\[([0-9]+)m".r
    private def extractNameComparisonObject[Comparison <: dsls.TransformedNamesComparison: Type]: Comparison = {
      // based on https://github.com/MateuszKubuszok/MacroTypeclass ideas
      object Comparison {
        def unapply(className: String): Option[Comparison] =
          try
            Option(Class.forName(className).getField("MODULE$").get(null).asInstanceOf[Comparison])
          catch {
            case _: Throwable => None
          }
      }

      // assuming this is "foo.bar.baz"...
      val name = AnsiControlCode.replaceAllIn(Type.prettyPrint[Comparison], "")

      Iterator
        .iterate(name + '$')(_.reverse.replaceFirst("[.]", "\\$").reverse)
        .take(name.count(_ == '.') + 1) // ...then this is: "foo.bar.baz$", "foo.bar$baz$", "foo$bar$baz$"...
        .toArray
        .reverse // ...and this is: "foo.bar.baz$", "foo.bar$baz$", "foo$bar$baz$"
        .collectFirst { case Comparison(value) => value } // attempts: top-level object, object in object, etc
        .getOrElse {
          // $COVERAGE-OFF$
          reportError(
            s"Invalid TransformerNamesComparison type - only (case) objects are allowed, and only the ones defined as top-level or in top-level objects, got: ${Type
                .prettyPrint[Comparison]}!!!"
          )
          // $COVERAGE-ON$
        }
    }
  }
}
