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
      nonUnitBeanSetters: Boolean = false,
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
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.NonUnitBeanSetters) {
        copy(nonUnitBeanSetters = value)
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

    def getFieldNameComparison: dsls.TransformedNamesComparison =
      fieldNameComparison.getOrElse(dsls.TransformedNamesComparison.FieldDefault)

    def setFieldNameComparison(nameComparison: Option[dsls.TransformedNamesComparison]): TransformerFlags =
      copy(fieldNameComparison = nameComparison)

    def getSubtypeNameComparison: dsls.TransformedNamesComparison =
      subtypeNameComparison.getOrElse(dsls.TransformedNamesComparison.SubtypeDefault)

    def setSubtypeNameComparison(nameComparison: Option[dsls.TransformedNamesComparison]): TransformerFlags =
      copy(subtypeNameComparison = nameComparison)

    override def toString: String = s"TransformerFlags(${Vector(
        if (inheritedAccessors) Vector("inheritedAccessors") else Vector.empty,
        if (methodAccessors) Vector("methodAccessors") else Vector.empty,
        if (processDefaultValues) Vector("processDefaultValues") else Vector.empty,
        if (beanSetters) Vector("beanSetters") else Vector.empty,
        if (beanSettersIgnoreUnmatched) Vector("beanSettersIgnoreUnmatched") else Vector.empty,
        if (nonUnitBeanSetters) Vector("nonUnitBeanSetters") else Vector.empty,
        if (beanGetters) Vector("beanGetters") else Vector.empty,
        if (optionDefaultsToNone) Vector("optionDefaultsToNone") else Vector.empty,
        implicitConflictResolution.map(r => s"ImplicitTransformerPreference=$r").toList.toVector,
        fieldNameComparison.map(r => s"fieldNameComparison=$r").toList.toVector,
        subtypeNameComparison.map(r => s"subtypeNameComparison=$r").toList.toVector,
        if (displayMacrosLogging) Vector("displayMacrosLogging") else Vector.empty
      ).flatten.mkString(", ")})"
  }

  final protected class Path private (private val segments: Vector[Path.Segment]) {

    import Path.Segment.*
    def select(name: String): Path = new Path(segments :+ Select(name))
    def `match`[Tpe: Type]: Path = new Path(segments :+ Match(Type[Tpe].as_??))

    @scala.annotation.tailrec
    def drop(prefix: Path)(implicit ctx: TransformationContext[?, ?]): Option[Path] = (prefix, this) match {
      case (Path.Root, result) => Some(result)
      case (Path.AtField(name1, prefix2), Path.AtField(name2, path2)) if areFieldNamesMatching(name1, name2) =>
        path2.drop(prefix2)
      case (Path.AtSubtype(tpe1, prefix2), Path.AtSubtype(tpe2, path2)) if tpe1.Underlying <:< tpe2.Underlying =>
        path2.drop(prefix2)
      case _ => None
    }

    override def equals(obj: Any): Boolean = obj match {
      case path: Path => segments == path.segments
      case _          => false
    }

    override def toString: String = "_" + segments.mkString
  }
  protected object Path {

    val Root = new Path(Vector())

    val clean = new Path(Vector(Segment.Clean))

    object AtField {
      def unapply(path: Path): Option[(String, Path)] =
        path.segments.headOption.collect { case Segment.Select(name) => name -> new Path(path.segments.tail) }
    }

    object AtSubtype {
      def unapply(path: Path): Option[(??, Path)] =
        path.segments.headOption.collect { case Segment.Match(tpe) => tpe -> new Path(path.segments.tail) }
    }

    sealed private trait Segment extends scala.Product with Serializable
    private object Segment {
      final case class Select(name: String) extends Segment {
        override def toString: String = s".$name"
      }
      final case class Match(tpe: ??) extends Segment {

        override def equals(obj: Any): Boolean = obj match {
          case Match(tpe2) => tpe.Underlying =:= tpe2.Underlying
          case _           => false
        }
        // TODO: figure out a better name
        override def toString: String = s".whenSubtype[${Type.prettyPrint(tpe.Underlying)}]"
      }
      case object Clean extends Segment
    }
  }

  sealed abstract protected class RuntimeOverride extends scala.Product with Serializable

  sealed abstract protected class RuntimeFieldOverride extends RuntimeOverride
  protected object RuntimeFieldOverride {
    final case class Const(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class ConstPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class Computed(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class ComputedPartial(runtimeDataIdx: Int) extends RuntimeFieldOverride
    final case class RenamedFrom(sourcePath: Path) extends RuntimeFieldOverride
  }

  sealed abstract protected class RuntimeCoproductOverride extends RuntimeOverride
  protected object RuntimeCoproductOverride {
    final case class CoproductInstance(runtimeDataIdx: Int) extends RuntimeCoproductOverride
    final case class CoproductInstancePartial(runtimeDataIdx: Int) extends RuntimeCoproductOverride
  }

  sealed abstract protected class RuntimeConstructorOverride extends RuntimeOverride
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

  final protected case class TransformerConfig(
      flags: TransformerFlags = TransformerFlags(),
      /** Let us distinct if flags were modified only by implicit TransformerConfiguration or maybe also locally */
      private val instanceFlagOverridden: Boolean = false,
      /** Stores all customizations provided by user */
      private val runtimeOverrides: Vector[(Path, RuntimeOverride)] = Vector.empty,
      /** Let us prevent `implicit val foo = foo` but allow `implicit val foo = new Foo { def sth = foo }` */
      private val preventImplicitSummoningForTypes: Option[(??, ??)] = None
  ) {

    private lazy val runtimeOverridesForCurrent = runtimeOverrides.filter {
      case (Path.AtField(_, Path.Root), _: RuntimeFieldOverride)       => true
      case (Path.AtSubtype(_, Path.Root), _: RuntimeCoproductOverride) => true
      case (Path.Root, _: RuntimeConstructorOverride)                  => true
      case _                                                           => false
    }

    def allowFromToImplicitSummoning: TransformerConfig =
      copy(preventImplicitSummoningForTypes = None)
    def preventImplicitSummoningFor[From: Type, To: Type]: TransformerConfig =
      copy(preventImplicitSummoningForTypes = Some(Type[From].as_?? -> Type[To].as_??))
    def isImplicitSummoningPreventedFor[From: Type, To: Type]: Boolean =
      preventImplicitSummoningForTypes.exists { case (someFrom, someTo) =>
        import someFrom.Underlying as SomeFrom, someTo.Underlying as SomeTo
        Type[SomeFrom] =:= Type[From] && Type[SomeTo] =:= Type[To]
      }

    def setLocalFlagsOverriden: TransformerConfig =
      copy(instanceFlagOverridden = true)
    def areLocalFlagsOverridesEmptyForCurrent: Boolean =
      !instanceFlagOverridden

    def addRuntimeOverride(path: Path, runtimeOverride: RuntimeOverride): TransformerConfig =
      copy(runtimeOverrides = runtimeOverrides :+ (path -> runtimeOverride))
    def areValueOverridesEmptyForCurrent: Boolean =
      runtimeOverrides.isEmpty
    def areOverridesEmptyForCurrent: Boolean =
      areLocalFlagsOverridesEmptyForCurrent && areValueOverridesEmptyForCurrent
    def filterOverridesForField(nameFilter: String => Boolean): Map[String, RuntimeFieldOverride] =
      ListMap.from(
        runtimeOverridesForCurrent.collect {
          case (Path.AtField(name, _), runtimeFieldOverride: RuntimeFieldOverride) if nameFilter(name) =>
            name -> runtimeFieldOverride
        }
      )
    def filterOverridesForCoproduct(typeFilter: ?? => Boolean): Map[??, RuntimeCoproductOverride] =
      ListMap.from(
        runtimeOverridesForCurrent.collect {
          case (Path.AtSubtype(tpe, _), runtimeCoproductOverride: RuntimeCoproductOverride) if typeFilter(tpe) =>
            tpe -> runtimeCoproductOverride
        }
      )
    def filterOverridesForConstructor: Option[RuntimeConstructorOverride] =
      runtimeOverridesForCurrent.collectFirst { case (_, runtimeConstructorOverride: RuntimeConstructorOverride) =>
        runtimeConstructorOverride
      }

    def prepareForRecursiveCall(toValuePath: Path)(implicit ctx: TransformationContext[?, ?]): TransformerConfig =
      copy(
        instanceFlagOverridden = false,
        runtimeOverrides = for {
          (path, runtimeOverride) <- runtimeOverrides
          alwaysDropOnRoot = runtimeOverride match {
            // Fields are always matched with "_.fieldName" Path while subtypes are always matched with
            // "_ match { case _: Tpe => }" so "_" Paths are useless in their case while they might get in way of
            // checking if there might be some relevant overrides for current/nested values
            case _: RuntimeFieldOverride | _: RuntimeCoproductOverride => true
            // Constructor is always matched at "_" Path, and dropped only when going inward
            case _: RuntimeConstructorOverride => false
          }
          newPath <- path.drop(toValuePath)
          if !(newPath == Path.Root && alwaysDropOnRoot)
        } yield newPath -> runtimeOverride,
        preventImplicitSummoningForTypes = None
      )

    override def toString: String = {
      val runtimeOverridesString =
        runtimeOverrides.map { case (path, runtimeOverride) => s"$path -> $runtimeOverride" }.mkString(", ")
      val preventImplicitSummoningForTypesString = preventImplicitSummoningForTypes.map { case (f, t) =>
        s"(${ExistentialType.prettyPrint(f)}, ${ExistentialType.prettyPrint(t)})"
      }.toString
      s"""TransformerConfig(
         |  flags = $flags,
         |  instanceFlagOverridden = $instanceFlagOverridden,
         |  runtimeOverrides = Vector($runtimeOverridesString),
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
      case ChimneyType.TransformerCfg.FieldConst(path, cfg) =>
        import path.Underlying as PathType, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addRuntimeOverride(
            extractPath[PathType],
            RuntimeFieldOverride.Const(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldConstPartial(path, cfg) =>
        import path.Underlying as PathType, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addRuntimeOverride(
            extractPath[PathType],
            RuntimeFieldOverride.ConstPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldComputed(path, cfg) =>
        import path.Underlying as PathType, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addRuntimeOverride(
            extractPath[PathType],
            RuntimeFieldOverride.Computed(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldComputedPartial(path, cfg) =>
        import path.Underlying as PathType, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addRuntimeOverride(
            extractPath[PathType],
            RuntimeFieldOverride.ComputedPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.FieldRelabelled(fromName, toName, cfg) =>
        import fromName.Underlying as FromName, toName.Underlying as ToName, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](runtimeDataIdx)
          .addRuntimeOverride(
            extractPath[ToName],
            RuntimeFieldOverride.RenamedFrom(extractPath[FromName])
          )
      case ChimneyType.TransformerCfg.CoproductInstance(instance, _, cfg) =>
        import instance.Underlying as Instance, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addRuntimeOverride(
            Path.Root.`match`[Instance],
            RuntimeCoproductOverride.CoproductInstance(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.CoproductInstancePartial(instance, _, cfg) =>
        import instance.Underlying as Instance, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addRuntimeOverride(
            Path.Root.`match`[Instance],
            RuntimeCoproductOverride.CoproductInstancePartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerCfg.Constructor(args, _, cfg) =>
        import args.Underlying as Args, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addRuntimeOverride(
            Path.Root,
            RuntimeConstructorOverride.Constructor(runtimeDataIdx, extractArgumentLists[Args])
          )
      case ChimneyType.TransformerCfg.ConstructorPartial(args, _, cfg) =>
        import args.Underlying as Args, cfg.Underlying as Cfg2
        extractTransformerConfig[Cfg2](1 + runtimeDataIdx)
          .addRuntimeOverride(
            Path.Root,
            RuntimeConstructorOverride.ConstructorPartial(runtimeDataIdx, extractArgumentLists[Args])
          )
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Invalid internal TransformerCfg type shape: ${Type.prettyPrint[Cfg]}!!")
      // $COVERAGE-ON$
    }

    private def extractPath[PathType <: runtime.Path: Type]: Path = Type[PathType] match {
      case root if root =:= ChimneyType.Path.Root =>
        Path.Root
      case ChimneyType.Path.Select(fieldName, path) =>
        import fieldName.Underlying as FieldName, path.Underlying as PathType2
        extractPath[PathType2].select(Type[FieldName].extractStringSingleton)
      case ChimneyType.Path.Match(subtype, path) =>
        import subtype.Underlying as Subtype, path.Underlying as PathType2
        extractPath[PathType2].`match`[Subtype]
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Invalid internal Path shape: ${Type.prettyPrint[PathType]}!!")
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
