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

    import Path.*, Path.Segment.*
    def select(name: String): Path = new Path(segments :+ Select(name))
    def `match`[Tpe: Type]: Path = new Path(segments :+ Match(Type[Tpe].as_??))
    def eachItem: Path = new Path(segments :+ EachItem)
    def eachMapKey: Path = new Path(segments :+ EachMapKey)
    def eachMapValue: Path = new Path(segments :+ EachMapValue)

    @scala.annotation.tailrec
    def drop(prefix: Path)(implicit ctx: TransformationContext[?, ?]): Option[Path] = (prefix, this) match {
      case (Root, result)                                                                         => Some(result)
      case (AtField(name, prefix2), AtField(name2, tail)) if areFieldNamesMatching(name, name2)   => tail.drop(prefix2)
      case (AtSubtype(tpe, prefix2), AtSubtype(tpe2, tail)) if tpe.Underlying <:< tpe2.Underlying => tail.drop(prefix2)
      case (AtItem(prefix2), AtItem(tail))                                                        => tail.drop(prefix2)
      case (AtMapKey(prefix2), AtMapKey(tail))                                                    => tail.drop(prefix2)
      case (AtMapValue(prefix2), AtMapValue(tail))                                                => tail.drop(prefix2)
      case _                                                                                      => None
    }

    override def equals(obj: Any): Boolean = obj match {
      case path: Path => segments == path.segments
      case _          => false
    }

    override def toString: String = "_" + segments.mkString
  }
  protected object Path {

    val Root = new Path(Vector())

    object AtField {
      def unapply(path: Path): Option[(String, Path)] =
        path.segments.headOption.collect { case Segment.Select(name) => name -> new Path(path.segments.tail) }
    }

    object AtSubtype {
      def unapply(path: Path): Option[(??, Path)] =
        path.segments.headOption.collect { case Segment.Match(tpe) => tpe -> new Path(path.segments.tail) }
    }

    object AtItem {
      def unapply(path: Path): Option[Path] =
        path.segments.headOption.collect { case Segment.EachItem => new Path(path.segments.tail) }
    }

    object AtMapKey {
      def unapply(path: Path): Option[Path] =
        path.segments.headOption.collect { case Segment.EachMapKey => new Path(path.segments.tail) }
    }

    object AtMapValue {
      def unapply(path: Path): Option[Path] =
        path.segments.headOption.collect { case Segment.EachMapValue => new Path(path.segments.tail) }
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

        override def toString: String = s".whenSubtype[${Type.prettyPrint(tpe.Underlying)}]"
      }
      case object EachItem extends Segment {
        override def toString: String = ".eachItem"
      }
      case object EachMapKey extends Segment {
        override def toString: String = ".eachMapKey"
      }
      case object EachMapValue extends Segment {
        override def toString: String = ".eachMapValue"
      }
    }
  }

  sealed protected trait TransformerOverride extends scala.Product with Serializable
  protected object TransformerOverride {
    sealed trait ForField extends TransformerOverride
    sealed trait ForSubtype extends TransformerOverride
    sealed trait ForConstructor extends TransformerOverride

    final case class Const(runtimeDataIdx: Int) extends ForField
    final case class ConstPartial(runtimeDataIdx: Int) extends ForField

    final case class Computed(runtimeDataIdx: Int) extends ForField // with ForSubtype
    final case class ComputedPartial(runtimeDataIdx: Int) extends ForField // with ForSubtype

    final case class CaseComputed(runtimeDataIdx: Int) extends ForSubtype
    final case class CaseComputedPartial(runtimeDataIdx: Int) extends ForSubtype

    type Args = List[ListMap[String, ??]]
    final case class Constructor(runtimeDataIdx: Int, args: Args) extends ForConstructor {
      override def toString: String = s"Constructor($runtimeDataIdx, ${printArgs(args)})"
    }
    final case class ConstructorPartial(runtimeDataIdx: Int, args: Args) extends ForConstructor {
      override def toString: String = s"ConstructorPartial($runtimeDataIdx, ${printArgs(args)})"
    }

    final case class RenamedFrom(sourcePath: Path) extends ForField

    private def printArgs(args: Args): String = {
      import ExistentialType.prettyPrint as printTpe
      if (args.isEmpty) "<no list>"
      else args.map(list => "(" + list.map { case (n, t) => s"$n: ${printTpe(t)}" }.mkString(", ") + ")").mkString
    }
  }

  final protected case class TransformerConfiguration(
      flags: TransformerFlags = TransformerFlags(),
      /** Let us distinct if flags were modified only by implicit TransformerConfiguration or maybe also locally */
      private val localFlagsOverridden: Boolean = false,
      /** Stores all customizations provided by user */
      private val runtimeOverrides: Vector[(Path, TransformerOverride)] = Vector.empty,
      /** Let us prevent `implicit val foo = foo` but allow `implicit val foo = new Foo { def sth = foo }` */
      private val preventImplicitSummoningForTypes: Option[(??, ??)] = None
  ) {

    private lazy val runtimeOverridesForCurrent = runtimeOverrides.filter {
      case (Path.AtField(_, Path.Root), _: TransformerOverride.ForField)     => true
      case (Path.AtSubtype(_, Path.Root), _: TransformerOverride.ForSubtype) => true
      case (Path.Root, _: TransformerOverride.ForConstructor)                => true
      case _                                                                 => false
    }

    def allowFromToImplicitSummoning: TransformerConfiguration =
      copy(preventImplicitSummoningForTypes = None)
    def preventImplicitSummoningFor[From: Type, To: Type]: TransformerConfiguration =
      copy(preventImplicitSummoningForTypes = Some(Type[From].as_?? -> Type[To].as_??))
    def isImplicitSummoningPreventedFor[From: Type, To: Type]: Boolean =
      preventImplicitSummoningForTypes.exists { case (someFrom, someTo) =>
        import someFrom.Underlying as SomeFrom, someTo.Underlying as SomeTo
        Type[SomeFrom] =:= Type[From] && Type[SomeTo] =:= Type[To]
      }

    def setLocalFlagsOverriden: TransformerConfiguration =
      copy(localFlagsOverridden = true)
    def areLocalFlagsEmpty: Boolean =
      !localFlagsOverridden

    def addTransformerOverride(path: Path, runtimeOverride: TransformerOverride): TransformerConfiguration =
      copy(runtimeOverrides = runtimeOverrides :+ (path -> runtimeOverride))
    def areOverridesEmpty: Boolean =
      runtimeOverrides.isEmpty
    def areLocalFlagsAndOverridesEmpty: Boolean =
      areLocalFlagsEmpty && areOverridesEmpty
    def filterCurrentOverridesForField(nameFilter: String => Boolean): Map[String, TransformerOverride.ForField] =
      ListMap.from(
        runtimeOverridesForCurrent.collect {
          case (Path.AtField(name, _), runtimeFieldOverride: TransformerOverride.ForField) if nameFilter(name) =>
            name -> runtimeFieldOverride
        }
      )
    def filterCurrentOverridesForSubtype(typeFilter: ?? => Boolean): Map[??, TransformerOverride.ForSubtype] =
      ListMap.from(
        runtimeOverridesForCurrent.collect {
          case (Path.AtSubtype(tpe, _), runtimeCoproductOverride: TransformerOverride.ForSubtype) if typeFilter(tpe) =>
            tpe -> runtimeCoproductOverride
        }
      )
    def currentOverrideForConstructor: Option[TransformerOverride.ForConstructor] =
      runtimeOverridesForCurrent.collectFirst {
        case (_, runtimeConstructorOverride: TransformerOverride.ForConstructor) => runtimeConstructorOverride
      }

    def prepareForRecursiveCall(toPath: Path)(implicit ctx: TransformationContext[?, ?]): TransformerConfiguration =
      copy(
        localFlagsOverridden = false,
        runtimeOverrides = for {
          (path, runtimeOverride) <- runtimeOverrides
          alwaysDropOnRoot = runtimeOverride match {
            // Fields are always matched with "_.fieldName" Path while subtypes are always matched with
            // "_ match { case _: Tpe => }" so "_" Paths are useless in their case while they might get in way of
            // checking if there might be some relevant overrides for current/nested values
            case _: TransformerOverride.ForField | _: TransformerOverride.ForSubtype => true
            // Constructor is always matched at "_" Path, and dropped only when going inward
            case _: TransformerOverride.ForConstructor => false
          }
          newPath <- path.drop(toPath)
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
         |  localFlagsOverridden = $localFlagsOverridden,
         |  runtimeOverrides = Vector($runtimeOverridesString),
         |  preventImplicitSummoningForTypes = $preventImplicitSummoningForTypesString
         |)""".stripMargin
    }
  }

  protected object TransformerConfigurations {

    // TODO: rename Config => Configuration

    final def readTransformerConfig[
        Tail <: runtime.TransformerOverrides: Type,
        InstanceFlags <: runtime.TransformerFlags: Type,
        ImplicitScopeFlags <: runtime.TransformerFlags: Type
    ]: TransformerConfiguration = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](TransformerFlags())
      val allFlags = extractTransformerFlags[InstanceFlags](implicitScopeFlags)
      val cfg = extractTransformerConfig[Tail](runtimeDataIdx = 0).copy(flags = allFlags)
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

    private def extractTransformerConfig[Tail <: runtime.TransformerOverrides: Type](
        runtimeDataIdx: Int
    ): TransformerConfiguration = Type[Tail] match {
      case empty if empty =:= ChimneyType.TransformerOverrides.Empty => TransformerConfiguration()
      case ChimneyType.TransformerOverrides.Const(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx)
          .addTransformerOverride(
            extractPath[ToPath],
            TransformerOverride.Const(runtimeDataIdx)
          )
      case ChimneyType.TransformerOverrides.ConstPartial(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx)
          .addTransformerOverride(
            extractPath[ToPath],
            TransformerOverride.ConstPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerOverrides.Computed(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx)
          .addTransformerOverride(
            extractPath[ToPath],
            TransformerOverride.Computed(runtimeDataIdx)
          )
      case ChimneyType.TransformerOverrides.ComputedPartial(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx)
          .addTransformerOverride(
            extractPath[ToPath],
            TransformerOverride.ComputedPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerOverrides.CaseComputed(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx)
          .addTransformerOverride(
            extractPath[ToPath],
            TransformerOverride.CaseComputed(runtimeDataIdx)
          )
      case ChimneyType.TransformerOverrides.CaseComputedPartial(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx)
          .addTransformerOverride(
            extractPath[ToPath],
            TransformerOverride.CaseComputedPartial(runtimeDataIdx)
          )
      case ChimneyType.TransformerOverrides.Constructor(args, toPath, cfg) =>
        import args.Underlying as Args, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx)
          .addTransformerOverride(
            extractPath[ToPath],
            TransformerOverride.Constructor(runtimeDataIdx, extractArgumentLists[Args])
          )
      case ChimneyType.TransformerOverrides.ConstructorPartial(args, toPath, cfg) =>
        import args.Underlying as Args, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx)
          .addTransformerOverride(
            extractPath[ToPath],
            TransformerOverride.ConstructorPartial(runtimeDataIdx, extractArgumentLists[Args])
          )
      case ChimneyType.TransformerOverrides.RenamedFrom(fromPath, toPath, cfg) =>
        import fromPath.Underlying as FromPath, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](runtimeDataIdx)
          .addTransformerOverride(
            extractPath[ToPath],
            TransformerOverride.RenamedFrom(extractPath[FromPath])
          )
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Invalid internal TransformerOverrides type shape: ${Type.prettyPrint[Tail]}!!")
      // $COVERAGE-ON$
    }

    private def extractPath[PathType <: runtime.Path: Type]: Path = Type[PathType] match {
      case root if root =:= ChimneyType.Path.Root =>
        Path.Root
      case ChimneyType.Path.Select(init, fieldName) =>
        import init.Underlying as PathType2, fieldName.Underlying as FieldName
        extractPath[PathType2].select(Type[FieldName].extractStringSingleton)
      case ChimneyType.Path.Match(init, subtype) =>
        import init.Underlying as PathType2, subtype.Underlying as Subtype
        extractPath[PathType2].`match`[Subtype]
      case ChimneyType.Path.EachItem(init) =>
        import init.Underlying as PathType2
        extractPath[PathType2].eachItem
      case ChimneyType.Path.EachMapKey(init) =>
        import init.Underlying as PathType2
        extractPath[PathType2].eachMapKey
      case ChimneyType.Path.EachMapValue(init) =>
        import init.Underlying as PathType2
        extractPath[PathType2].eachMapValue
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
