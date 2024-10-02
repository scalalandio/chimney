package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, TransformerDefinitionCommons}
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime

import scala.collection.compat.*
import scala.collection.immutable.{ListMap, ListSet}

private[compiletime] trait Configurations { this: Derivation =>

  import Type.Implicits.*

  final protected case class TransformerFlags(
      inheritedAccessors: Boolean = false,
      methodAccessors: Boolean = false,
      processDefaultValues: Boolean = false,
      processDefaultValuesOfType: ListSet[??] = ListSet.empty,
      beanSetters: Boolean = false,
      beanSettersIgnoreUnmatched: Boolean = false,
      nonUnitBeanSetters: Boolean = false,
      beanGetters: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      partialUnwrapsOption: Boolean = true,
      nonAnyValWrappers: Boolean = false,
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
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.NonAnyValWrappers) {
        copy(nonAnyValWrappers = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.MacrosLogging) {
        copy(displayMacrosLogging = value)
      } else {
        // $COVERAGE-OFF$
        reportError(s"Invalid internal TransformerFlag type: ${Type[Flag]}!")
        // $COVERAGE-ON$
      }

    def setDefaultValueOfType[A: Type](value: Boolean): TransformerFlags =
      copy(processDefaultValuesOfType =
        if (value) processDefaultValuesOfType + Type[A].as_??
        else processDefaultValuesOfType.filterNot(_.Underlying =:= Type[A])
      )

    def getDefaultValueOfType[A: Type]: Boolean =
      processDefaultValuesOfType.exists(_.Underlying =:= Type[A])

    def isDefaultValueEnabledGloballyOrFor[A: Type]: Boolean =
      processDefaultValues || getDefaultValueOfType[A]

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
        if (processDefaultValuesOfType.nonEmpty) Vector(processDefaultValuesOfType.toVector.map(ExistentialType.prettyPrint).mkString("processDefaultValuesOfType=(", ", ", ")"))
        else Vector.empty,
        if (beanSetters) Vector("beanSetters") else Vector.empty,
        if (beanSettersIgnoreUnmatched) Vector("beanSettersIgnoreUnmatched") else Vector.empty,
        if (nonUnitBeanSetters) Vector("nonUnitBeanSetters") else Vector.empty,
        if (beanGetters) Vector("beanGetters") else Vector.empty,
        if (optionDefaultsToNone) Vector("optionDefaultsToNone") else Vector.empty,
        if (nonAnyValWrappers) Vector("nonAnyValWrappers") else Vector.empty,
        implicitConflictResolution.map(r => s"ImplicitTransformerPreference=$r").toList.toVector,
        fieldNameComparison.map(r => s"fieldNameComparison=$r").toList.toVector,
        subtypeNameComparison.map(r => s"subtypeNameComparison=$r").toList.toVector,
        if (displayMacrosLogging) Vector("displayMacrosLogging") else Vector.empty
      ).flatten.mkString(", ")})"
  }
  object TransformerFlags {

    // $COVERAGE-OFF$It's testable in (Scala-CLI) snippets and not really in normal tests with coverage
    def global: TransformerFlags = XMacroSettings.foldLeft(TransformerFlags()) {
      case (cfg, transformerFlag"InheritedAccessors=$value") => cfg.copy(inheritedAccessors = value.toBoolean)
      case (cfg, transformerFlag"MethodAccessors=$value")    => cfg.copy(methodAccessors = value.toBoolean)
      case (cfg, transformerFlag"DefaultValues=$value")      => cfg.copy(processDefaultValues = value.toBoolean)
      case (cfg, transformerFlag"BeanSetters=$value")        => cfg.copy(beanSetters = value.toBoolean)
      case (cfg, transformerFlag"BeanSettersIgnoreUnmatched=$value") =>
        cfg.copy(beanSettersIgnoreUnmatched = value.toBoolean)
      case (cfg, transformerFlag"NonUnitBeanSetters=$value")   => cfg.copy(nonUnitBeanSetters = value.toBoolean)
      case (cfg, transformerFlag"BeanGetters=$value")          => cfg.copy(beanGetters = value.toBoolean)
      case (cfg, transformerFlag"OptionDefaultsToNone=$value") => cfg.copy(optionDefaultsToNone = value.toBoolean)
      case (cfg, transformerFlag"PartialUnwrapsOption=$value") => cfg.copy(partialUnwrapsOption = value.toBoolean)
      case (cfg, transformerFlag"NonAnyValWrappers=$value")    => cfg.copy(nonAnyValWrappers = value.toBoolean)
      case (cfg, transformerFlag"ImplicitConflictResolution=$value") =>
        cfg.copy(implicitConflictResolution = value match {
          case "PreferTotalTransformer"   => Some(dsls.PreferTotalTransformer)
          case "PreferPartialTransformer" => Some(dsls.PreferPartialTransformer)
          case "none"                     => None
        })
      case (cfg, transformerFlag"MacrosLogging=$value") => cfg.copy(displayMacrosLogging = value.toBoolean)
      case (cfg, _)                                     => cfg
    }
    // $COVERAGE-ON$
  }

  final protected class Path private (private val segments: Vector[Path.Segment]) {

    import Path.*, Path.Segment.*
    def select(name: String): Path = new Path(segments :+ Select(name))
    def matching[Tpe: Type]: Path = new Path(segments :+ Matching(Type[Tpe].as_??))
    def sourceMatching[Tpe: Type]: Path = new Path(segments :+ SourceMatching(Type[Tpe].as_??))
    def everyItem: Path = new Path(segments :+ EveryItem)
    def everyMapKey: Path = new Path(segments :+ EveryMapKey)
    def everyMapValue: Path = new Path(segments :+ EveryMapValue)

    @scala.annotation.tailrec
    def drop(prefix: Path)(implicit ctx: TransformationContext[?, ?]): Option[Path] = (prefix, this) match {
      case (Root, result)                                                                         => Some(result)
      case (AtField(name, prefix2), AtField(name2, tail)) if areFieldNamesMatching(name, name2)   => tail.drop(prefix2)
      case (AtSubtype(tpe, prefix2), AtSubtype(tpe2, tail)) if tpe.Underlying <:< tpe2.Underlying => tail.drop(prefix2)
      case (AtSubtype(_, prefix2), AtField(_, _))                                                 => this.drop(prefix2)
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

    def apply(selector: Path => Path): Path = selector(Root)

    object AtField {
      def unapply(path: Path): Option[(String, Path)] =
        path.segments.headOption.collect { case Segment.Select(name) => name -> new Path(path.segments.tail) }
    }

    object AtSubtype {
      def unapply(path: Path): Option[(??, Path)] =
        path.segments.headOption.collect { case Segment.Matching(tpe) => tpe -> new Path(path.segments.tail) }
    }

    object AtItem {
      def unapply(path: Path): Option[Path] =
        path.segments.headOption.collect { case Segment.EveryItem => new Path(path.segments.tail) }
    }

    object AtMapKey {
      def unapply(path: Path): Option[Path] =
        path.segments.headOption.collect { case Segment.EveryMapKey => new Path(path.segments.tail) }
    }

    object AtMapValue {
      def unapply(path: Path): Option[Path] =
        path.segments.headOption.collect { case Segment.EveryMapValue => new Path(path.segments.tail) }
    }

    sealed private trait Segment extends scala.Product with Serializable
    private object Segment {
      final case class Select(name: String) extends Segment {
        override def toString: String = s".$name"
      }
      final case class Matching(tpe: ??) extends Segment {
        override def equals(obj: Any): Boolean = obj match {
          case Matching(tpe2) => tpe.Underlying =:= tpe2.Underlying
          case _              => false
        }

        override def toString: String = s".matching[${Type.prettyPrint(tpe.Underlying)}]"
      }
      final case class SourceMatching(tpe: ??) extends Segment {
        override def equals(obj: Any): Boolean = obj match {
          case Matching(tpe2) => tpe.Underlying =:= tpe2.Underlying
          case _              => false
        }

        override def toString: String = s" if src.isInstanceOf[${Type.prettyPrint(tpe.Underlying)}]"
      }
      case object EveryItem extends Segment {
        override def toString: String = ".everyItem"
      }
      case object EveryMapKey extends Segment {
        override def toString: String = ".everyMapKey"
      }
      case object EveryMapValue extends Segment {
        override def toString: String = ".everyMapValue"
      }
    }
  }

  sealed protected trait SidedPath extends scala.Product with Serializable {
    def path: Path = this match {
      case SourcePath(fromPath) => fromPath
      case TargetPath(toPath)   => toPath
    }
    def drop(droppedFrom: Path, droppedTo: Path)(implicit ctx: TransformationContext[?, ?]): Option[SidedPath] =
      this match {
        case SourcePath(fromPath) => fromPath.drop(droppedFrom).map(SourcePath(_))
        case TargetPath(toPath)   => toPath.drop(droppedTo).map(TargetPath(_))
      }
  }
  protected object SidedPath {

    def unapply(sidedPath: SidedPath): Some[Path] = sidedPath match {
      case SourcePath(fromPath) => Some(fromPath)
      case TargetPath(toPath)   => Some(toPath)
    }
  }
  final protected case class SourcePath(fromPath: Path) extends SidedPath {
    override def toString: String = s"Source at $fromPath"
  }
  final protected case class TargetPath(toPath: Path) extends SidedPath {
    override def toString: String = s"Target at $toPath"
  }

  sealed protected trait TransformerOverride extends scala.Product with Serializable
  protected object TransformerOverride {
    sealed trait ForField extends TransformerOverride
    sealed trait ForSubtype extends TransformerOverride
    sealed trait ForConstructor extends TransformerOverride

    final case class Const(runtimeData: Expr[Any]) extends ForField {
      override def toString: String = s"Const(${Expr.prettyPrint(runtimeData)})"
    }
    final case class ConstPartial(runtimeData: Expr[Any]) extends ForField {
      override def toString: String = s"ConstPartial(${Expr.prettyPrint(runtimeData)})"
    }

    final case class Computed(runtimeData: Expr[Any]) extends ForField {
      override def toString: String = s"Computed(${Expr.prettyPrint(runtimeData)})"
    }
    final case class ComputedPartial(runtimeData: Expr[Any]) extends ForField {
      override def toString: String = s"ComputedPartial(${Expr.prettyPrint(runtimeData)})"
    }

    final case class CaseComputed(runtimeData: Expr[Any]) extends ForSubtype {
      override def toString: String = s"CaseComputed(${Expr.prettyPrint(runtimeData)})"
    }
    final case class CaseComputedPartial(runtimeData: Expr[Any]) extends ForSubtype {
      override def toString: String = s"CaseComputedPartial(${Expr.prettyPrint(runtimeData)})"
    }

    type Args = List[ListMap[String, ??]]
    final case class Constructor(runtimeData: Expr[Any], args: Args) extends ForConstructor {
      override def toString: String = s"Constructor(${Expr.prettyPrint(runtimeData)}, ${printArgs(args)})"
    }
    final case class ConstructorPartial(runtimeData: Expr[Any], args: Args) extends ForConstructor {
      override def toString: String = s"ConstructorPartial(${Expr.prettyPrint(runtimeData)}, ${printArgs(args)})"
    }

    final case class RenamedFrom(sourcePath: Path) extends ForField
    final case class RenamedTo(targetPath: Path) extends ForSubtype

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
      private val runtimeOverrides: Vector[(SidedPath, TransformerOverride)] = Vector.empty,
      /** Let us prevent `implicit val foo = foo` but allow `implicit val foo = new Foo { def sth = foo }` */
      private val preventImplicitSummoningForTypes: Option[(??, ??)] = None
  ) {

    private lazy val runtimeOverridesForCurrent = runtimeOverrides.filter {
      case (SidedPath(Path.AtField(_, Path.Root)), _: TransformerOverride.ForField)     => true
      case (SidedPath(Path.AtSubtype(_, Path.Root)), _: TransformerOverride.ForSubtype) => true
      case (SidedPath(Path.Root), _: TransformerOverride.ForConstructor)                => true
      case _                                                                            => false
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

    def addTransformerOverride(sidedPath: SidedPath, runtimeOverride: TransformerOverride): TransformerConfiguration =
      copy(runtimeOverrides = runtimeOverrides :+ (sidedPath -> runtimeOverride))
    def areOverridesEmpty: Boolean =
      runtimeOverrides.isEmpty
    def areLocalFlagsAndOverridesEmpty: Boolean =
      areLocalFlagsEmpty && areOverridesEmpty

    def filterCurrentOverridesForField(nameFilter: String => Boolean): Map[String, TransformerOverride.ForField] =
      ListMap.from(
        runtimeOverridesForCurrent.collect {
          case (TargetPath(Path.AtField(name, _)), runtimeFieldOverride: TransformerOverride.ForField)
              if nameFilter(name) =>
            name -> runtimeFieldOverride
        }
      )
    def filterCurrentOverridesForSubtype(
        sourceTypeFilter: ?? => Boolean
    ): Map[??, TransformerOverride.ForSubtype] = ListMap.from(
      runtimeOverridesForCurrent.collect {
        case (SourcePath(Path.AtSubtype(tpe, _)), runtimeCoproductOverride: TransformerOverride.ForSubtype)
            if sourceTypeFilter(tpe) =>
          tpe -> runtimeCoproductOverride
      }
    )
    def filterCurrentOverridesForSome: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (TargetPath(Path.AtSubtype(tpe, path)), runtimeFieldOverride: TransformerOverride.ForField)
            if path == Path.Root && tpe.Underlying <:< Type[Some[Any]] =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForLeft: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (TargetPath(Path.AtSubtype(tpe, path)), runtimeFieldOverride: TransformerOverride.ForField)
            if path == Path.Root && tpe.Underlying <:< Type[Left[Any, Any]] =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForRight: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (TargetPath(Path.AtSubtype(tpe, path)), runtimeFieldOverride: TransformerOverride.ForField)
            if path == Path.Root && tpe.Underlying <:< Type[Right[Any, Any]] =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForEveryItem: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (TargetPath(Path.AtItem(path)), runtimeFieldOverride: TransformerOverride.ForField) if path == Path.Root =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForEveryMapKey: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (TargetPath(Path.AtMapKey(path)), runtimeFieldOverride: TransformerOverride.ForField)
            if path == Path.Root =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForEveryMapValue: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (TargetPath(Path.AtMapValue(path)), runtimeFieldOverride: TransformerOverride.ForField)
            if path == Path.Root =>
          runtimeFieldOverride
      }
    )
    def currentOverrideForConstructor: Option[TransformerOverride.ForConstructor] =
      runtimeOverridesForCurrent.collectFirst {
        case (TargetPath(_), runtimeConstructorOverride: TransformerOverride.ForConstructor) =>
          runtimeConstructorOverride
      }

    def prepareForRecursiveCall(fromPath: Path, toPath: Path)(implicit
        ctx: TransformationContext[?, ?]
    ): TransformerConfiguration =
      copy(
        localFlagsOverridden = false,
        runtimeOverrides = for {
          (sidedPath, runtimeOverride) <- runtimeOverrides
          alwaysDropOnRoot = runtimeOverride match {
            // Fields are always matched with "_.fieldName" Path while subtypes are always matched with
            // "_ match { case _: Tpe => }" so "_" Paths are useless in their case while they might get in way of
            // checking if there might be some relevant overrides for current/nested values
            case _: TransformerOverride.ForField | _: TransformerOverride.ForSubtype => true
            // Constructor is always matched at "_" Path, and dropped only when going inward
            case _: TransformerOverride.ForConstructor => false
          }
          newSidePath <- sidedPath.drop(fromPath, toPath).to(Vector)
          if !(newSidePath.path == Path.Root && alwaysDropOnRoot)
        } yield newSidePath -> runtimeOverride,
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

    final def readTransformerConfiguration[
        Tail <: runtime.TransformerOverrides: Type,
        InstanceFlags <: runtime.TransformerFlags: Type,
        ImplicitScopeFlags <: runtime.TransformerFlags: Type
    ](runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]): TransformerConfiguration = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](TransformerFlags.global)
      val allFlags = extractTransformerFlags[InstanceFlags](implicitScopeFlags)
      val cfg = extractTransformerConfig[Tail](runtimeDataIdx = 0, runtimeDataStore).copy(flags = allFlags)
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
          case ChimneyType.TransformerFlags.Flags.DefaultValueOfType(t) =>
            import t.Underlying as T
            extractTransformerFlags[Flags2](defaultFlags).setDefaultValueOfType[T](value = true)
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
              // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
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
          case ChimneyType.TransformerFlags.Flags.DefaultValueOfType(t) =>
            import t.Underlying as T
            extractTransformerFlags[Flags2](defaultFlags).setDefaultValueOfType[T](value = false)
          case ChimneyType.TransformerFlags.Flags.ImplicitConflictResolution(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setImplicitConflictResolution(None)
          case ChimneyType.TransformerFlags.Flags.FieldNameComparison(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setFieldNameComparison(None)
          case ChimneyType.TransformerFlags.Flags.SubtypeNameComparison(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setSubtypeNameComparison(None)
          case _ =>
            extractTransformerFlags[Flags2](defaultFlags).setBoolFlag[Flag](value = false)
        }
      // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
      case _ =>
        reportError(s"Invalid internal TransformerFlags type shape: ${Type.prettyPrint[Flags]}!")
      // $COVERAGE-ON$
    }

    private def extractTransformerConfig[Tail <: runtime.TransformerOverrides: Type](
        runtimeDataIdx: Int,
        runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
    ): TransformerConfiguration = Type[Tail] match {
      case empty if empty =:= ChimneyType.TransformerOverrides.Empty => TransformerConfiguration()
      case ChimneyType.TransformerOverrides.Const(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore)
          .addTransformerOverride(
            TargetPath(extractPath[ToPath]),
            TransformerOverride.Const(runtimeDataStore(runtimeDataIdx))
          )
      case ChimneyType.TransformerOverrides.ConstPartial(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore)
          .addTransformerOverride(
            TargetPath(extractPath[ToPath]),
            TransformerOverride.ConstPartial(runtimeDataStore(runtimeDataIdx))
          )
      case ChimneyType.TransformerOverrides.Computed(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore)
          .addTransformerOverride(
            TargetPath(extractPath[ToPath]),
            TransformerOverride.Computed(runtimeDataStore(runtimeDataIdx))
          )
      case ChimneyType.TransformerOverrides.ComputedPartial(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore)
          .addTransformerOverride(
            TargetPath(extractPath[ToPath]),
            TransformerOverride.ComputedPartial(runtimeDataStore(runtimeDataIdx))
          )
      case ChimneyType.TransformerOverrides.CaseComputed(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore)
          .addTransformerOverride(
            SourcePath(extractPath[ToPath]),
            TransformerOverride.CaseComputed(runtimeDataStore(runtimeDataIdx))
          )
      case ChimneyType.TransformerOverrides.CaseComputedPartial(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore)
          .addTransformerOverride(
            SourcePath(extractPath[ToPath]),
            TransformerOverride.CaseComputedPartial(runtimeDataStore(runtimeDataIdx))
          )
      case ChimneyType.TransformerOverrides.Constructor(args, toPath, cfg) =>
        import args.Underlying as Args, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore)
          .addTransformerOverride(
            TargetPath(extractPath[ToPath]),
            TransformerOverride.Constructor(runtimeDataStore(runtimeDataIdx), extractArgumentLists[Args])
          )
      case ChimneyType.TransformerOverrides.ConstructorPartial(args, toPath, cfg) =>
        import args.Underlying as Args, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore)
          .addTransformerOverride(
            TargetPath(extractPath[ToPath]),
            TransformerOverride.ConstructorPartial(runtimeDataStore(runtimeDataIdx), extractArgumentLists[Args])
          )
      case ChimneyType.TransformerOverrides.RenamedFrom(fromPath, toPath, cfg) =>
        import fromPath.Underlying as FromPath, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](runtimeDataIdx, runtimeDataStore)
          .addTransformerOverride(
            TargetPath(extractPath[ToPath]),
            TransformerOverride.RenamedFrom(extractPath[FromPath])
          )
      case ChimneyType.TransformerOverrides.RenamedTo(fromPath, toPath, cfg) =>
        import fromPath.Underlying as FromPath, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](runtimeDataIdx, runtimeDataStore)
          .addTransformerOverride(
            SourcePath(extractPath[FromPath]),
            TransformerOverride.RenamedTo(extractPath[ToPath])
          )
      // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
      case _ =>
        reportError(s"Invalid internal TransformerOverrides type shape: ${Type.prettyPrint[Tail]}!!")
      // $COVERAGE-ON$
    }

    private def extractPath[PathType <: runtime.Path: Type]: Path = Type[PathType] match {
      case root if root =:= ChimneyType.Path.Root =>
        Path.Root
      case ChimneyType.Path.Select(init, fieldName) =>
        import init.Underlying as PathType2, fieldName.Underlying as FieldName
        extractPath[PathType2].select(Type[FieldName].extractStringSingleton)
      case ChimneyType.Path.Matching(init, subtype) =>
        import init.Underlying as PathType2, subtype.Underlying as Subtype
        extractPath[PathType2].matching[Subtype]
      case ChimneyType.Path.SourceMatching(init, sourceSubtype) =>
        import init.Underlying as PathType2, sourceSubtype.Underlying as SourceSubtype
        extractPath[PathType2].matching[SourceSubtype]
      case ChimneyType.Path.EveryItem(init) =>
        import init.Underlying as PathType2
        extractPath[PathType2].everyItem
      case ChimneyType.Path.EveryMapKey(init) =>
        import init.Underlying as PathType2
        extractPath[PathType2].everyMapKey
      case ChimneyType.Path.EveryMapValue(init) =>
        import init.Underlying as PathType2
        extractPath[PathType2].everyMapValue
      // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
      case _ =>
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
          // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
          reportError(
            s"Invalid TransformerNamesComparison type - only (case) objects are allowed, and only the ones defined as top-level or in top-level objects, got: ${Type
                .prettyPrint[Comparison]}!!!"
          )
          // $COVERAGE-ON$
        }
    }
  }
}
