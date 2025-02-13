package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.PatcherDefinitionCommons
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime

import scala.collection.compat.*
import scala.collection.immutable.{ListMap, ListSet}
import io.scalaland.chimney.dsl.UnusedFieldPolicy
import io.scalaland.chimney.dsl.UnmatchedSubtypePolicy

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
      typeConstraintEvidence: Boolean = false,
      implicitConversions: Boolean = false,
      implicitConflictResolution: Option[dsls.ImplicitTransformerPreference] = None,
      optionFallbackMerge: Option[dsls.OptionFallbackMergeStrategy] = None,
      eitherFallbackMerge: Option[dsls.OptionFallbackMergeStrategy] = None,
      collectionFallbackMerge: Option[dsls.CollectionFallbackMergeStrategy] = None,
      fieldNameComparison: Option[dsls.TransformedNamesComparison] = None,
      subtypeNameComparison: Option[dsls.TransformedNamesComparison] = None,
      unusedFieldPolicy: Option[dsls.UnusedFieldPolicy] = None,
      unmatchedSubtypePolicy: Option[dsls.UnmatchedSubtypePolicy] = None,
      displayMacrosLogging: Boolean = false,
      scopedUpdates: List[(SidedPath, TransformerFlags => TransformerFlags)] = List.empty
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
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.TypeConstraintEvidence) {
        copy(typeConstraintEvidence = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.ImplicitConversions) {
        copy(implicitConversions = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.MacrosLogging) {
        copy(displayMacrosLogging = value)
      } else {
        // $COVERAGE-OFF$
        reportError(s"Invalid internal TransformerFlag type: ${Type.prettyPrint[Flag]}!")
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

    def setImplicitConflictResolution(preference: Option[dsls.ImplicitTransformerPreference]): TransformerFlags =
      copy(implicitConflictResolution = preference)

    def setOptionFallbackMerge(strategy: Option[dsls.OptionFallbackMergeStrategy]): TransformerFlags =
      copy(optionFallbackMerge = strategy)

    def setEitherFallbackMerge(strategy: Option[dsls.OptionFallbackMergeStrategy]): TransformerFlags =
      copy(eitherFallbackMerge = strategy)

    def setCollectionFallbackMerge(strategy: Option[dsls.CollectionFallbackMergeStrategy]): TransformerFlags =
      copy(collectionFallbackMerge = strategy)

    def getFieldNameComparison: dsls.TransformedNamesComparison =
      fieldNameComparison.getOrElse(dsls.TransformedNamesComparison.FieldDefault)

    def setFieldNameComparison(nameComparison: Option[dsls.TransformedNamesComparison]): TransformerFlags =
      copy(fieldNameComparison = nameComparison)

    def getSubtypeNameComparison: dsls.TransformedNamesComparison =
      subtypeNameComparison.getOrElse(dsls.TransformedNamesComparison.SubtypeDefault)

    def setSubtypeNameComparison(nameComparison: Option[dsls.TransformedNamesComparison]): TransformerFlags =
      copy(subtypeNameComparison = nameComparison)

    def setUnusedFieldPolicy(policy: Option[UnusedFieldPolicy]): TransformerFlags =
      copy(unusedFieldPolicy = policy)

    def setUnmatchedSubtypePolicy(policy: Option[UnmatchedSubtypePolicy]): TransformerFlags =
      copy(unmatchedSubtypePolicy = policy)

    def setSourceFlags(sourcePath: Path)(update: TransformerFlags => TransformerFlags): TransformerFlags =
      copy(scopedUpdates = scopedUpdates :+ (SourcePath(sourcePath) -> update))

    def setTargetFlags(targetPath: Path)(update: TransformerFlags => TransformerFlags): TransformerFlags =
      copy(scopedUpdates = scopedUpdates :+ (TargetPath(targetPath) -> update))

    def at(prefix: SidedPath)(implicit ctx: TransformationContext[?, ?]): TransformerFlags = prefix match {
      case SourcePath(fromPath) => prepareForRecursiveCall(fromPath, Path.Root)
      case TargetPath(toPath)   => prepareForRecursiveCall(Path.Root, toPath)
    }
    def atSrc(selector: Path => Path)(implicit ctx: TransformationContext[?, ?]): TransformerFlags = at(
      SourcePath(selector)
    )
    def atTgt(selector: Path => Path)(implicit ctx: TransformationContext[?, ?]): TransformerFlags = at(
      TargetPath(selector)
    )

    def prepareForRecursiveCall(fromPath: Path, toPath: Path)(implicit
        ctx: TransformationContext[?, ?]
    ): TransformerFlags = {
      val (immediate, nested) = scopedUpdates.view
        .flatMap { case (path, update) => path.drop(fromPath, toPath).map(_ -> update) }
        .partition(_._1.path == Path.Root)
      immediate.map(_._2).foldLeft(this)((f, u) => u(f)).copy(scopedUpdates = nested.toList)
    }

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
        if (typeConstraintEvidence) Vector("typeConstraintEvidence") else Vector.empty,
        if (implicitConversions) Vector("implicitConversions") else Vector.empty,
        implicitConflictResolution.map(r => s"ImplicitTransformerPreference=$r").toList.toVector,
        optionFallbackMerge.map(s => s"optionFallbackMerge=$s").toList.toVector,
        eitherFallbackMerge.map(s => s"eitherFallbackMerge=$s").toList.toVector,
        collectionFallbackMerge.map(s => s"collectionFallbackMerge=$s").toList.toVector,
        fieldNameComparison.map(c => s"fieldNameComparison=$c").toList.toVector,
        subtypeNameComparison.map(c => s"subtypeNameComparison=$c").toList.toVector,
        unusedFieldPolicy.map(p => s"unusedFieldPolicy=$p").toList.toVector,
        unmatchedSubtypePolicy.map(p => s"unmatchedSubtypePolicy=$p").toList.toVector,
        if (displayMacrosLogging) Vector("displayMacrosLogging") else Vector.empty,
        if (scopedUpdates.nonEmpty) Vector(scopedUpdates.map(_._1).mkString("scopedUpdates=(", ", ", ")"))
        else Vector.empty
      ).flatten.mkString(", ")})"
  }
  protected object TransformerFlags {

    // $COVERAGE-OFF$It's testable in (Scala-CLI) snippets and not really in normal tests with coverage
    def global: TransformerFlags = XMacroSettings.foldLeft(TransformerFlags()) {
      case (cfg, transformerFlag"InheritedAccessors=$value") => cfg.copy(inheritedAccessors = value.toBoolean)
      case (cfg, transformerFlag"MethodAccessors=$value")    => cfg.copy(methodAccessors = value.toBoolean)
      case (cfg, transformerFlag"DefaultValues=$value")      => cfg.copy(processDefaultValues = value.toBoolean)
      case (cfg, transformerFlag"BeanSetters=$value")        => cfg.copy(beanSetters = value.toBoolean)
      case (cfg, transformerFlag"BeanSettersIgnoreUnmatched=$value") =>
        cfg.copy(beanSettersIgnoreUnmatched = value.toBoolean)
      case (cfg, transformerFlag"NonUnitBeanSetters=$value")     => cfg.copy(nonUnitBeanSetters = value.toBoolean)
      case (cfg, transformerFlag"BeanGetters=$value")            => cfg.copy(beanGetters = value.toBoolean)
      case (cfg, transformerFlag"OptionDefaultsToNone=$value")   => cfg.copy(optionDefaultsToNone = value.toBoolean)
      case (cfg, transformerFlag"PartialUnwrapsOption=$value")   => cfg.copy(partialUnwrapsOption = value.toBoolean)
      case (cfg, transformerFlag"NonAnyValWrappers=$value")      => cfg.copy(nonAnyValWrappers = value.toBoolean)
      case (cfg, transformerFlag"TypeConstraintEvidence=$value") => cfg.copy(typeConstraintEvidence = value.toBoolean)
      case (cfg, transformerFlag"ImplicitConversions=$value")    => cfg.copy(implicitConversions = value.toBoolean)
      case (cfg, transformerFlag"ImplicitConflictResolution=$value") =>
        cfg.copy(implicitConflictResolution = value match {
          case "PreferTotalTransformer"   => Some(dsls.PreferTotalTransformer)
          case "PreferPartialTransformer" => Some(dsls.PreferPartialTransformer)
          case "none"                     => None
        })
      case (cfg, transformerFlag"OptionFallbackMerge=$value") =>
        cfg.copy(optionFallbackMerge = value match {
          case "SourceOrElseFallback" => Some(dsls.SourceOrElseFallback)
          case "FallbackOrElseSource" => Some(dsls.FallbackOrElseSource)
          case "none"                 => None
        })
      case (cfg, transformerFlag"EitherFallbackMerge=$value") =>
        cfg.copy(eitherFallbackMerge = value match {
          case "SourceOrElseFallback" => Some(dsls.SourceOrElseFallback)
          case "FallbackOrElseSource" => Some(dsls.FallbackOrElseSource)
          case "none"                 => None
        })
      case (cfg, transformerFlag"CollectionFallbackMerge=$value") =>
        cfg.copy(collectionFallbackMerge = value match {
          case "SourceAppendFallback" => Some(dsls.SourceAppendFallback)
          case "FallbackAppendSource" => Some(dsls.FallbackAppendSource)
          case "none"                 => None
        })
      case (cfg, transformerFlag"UnusedFieldPolicy=$value") =>
        cfg.copy(unusedFieldPolicy = value match {
          case "FailOnIgnoredSourceVal" => Some(dsls.FailOnIgnoredSourceVal)
          case "none"                   => None
        })
      case (cfg, transformerFlag"UnmatchedSubtypePolicy=$value") =>
        cfg.copy(unmatchedSubtypePolicy = value match {
          case "FailOnUnmatchedTargetSubtype" => Some(dsls.FailOnUnmatchedTargetSubtype)
          case "none"                         => None
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
    def everyItem: Path = new Path(segments :+ EveryItem)
    def everyMapKey: Path = new Path(segments :+ EveryMapKey)
    def everyMapValue: Path = new Path(segments :+ EveryMapValue)

    def concat(path: Path): Path = new Path(segments ++ path.segments)

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
      def unapply(path: Path): Option[Path] = path.segments match {
        case Segment.EveryItem +: rest     => Some(new Path(rest))
        case Segment.EveryMapKey +: rest   => Some(new Path(Segment.Select("_1") +: rest))
        case Segment.EveryMapValue +: rest => Some(new Path(Segment.Select("_2") +: rest))
        case _                             => None
      }
    }

    object AtMapKey {
      def unapply(path: Path): Option[Path] = path.segments match {
        case Segment.EveryMapKey +: rest                       => Some(new Path(rest))
        case Segment.EveryItem +: Segment.Select("_1") +: rest => Some(new Path(rest))
        case _                                                 => None
      }
    }

    object AtMapValue {
      def unapply(path: Path): Option[Path] = path.segments match {
        case Segment.EveryMapValue +: rest                     => Some(new Path(rest))
        case Segment.EveryItem +: Segment.Select("_2") +: rest => Some(new Path(rest))
        case _                                                 => None
      }
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
    def drop(prefix: SidedPath)(implicit ctx: TransformationContext[?, ?]): Option[SidedPath] = prefix match {
      case SourcePath(fromPath) => drop(fromPath, Path.Root)
      case TargetPath(toPath)   => drop(Path.Root, toPath)
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
  protected object SourcePath {
    def apply(selector: Path => Path): SourcePath = new SourcePath(Path(selector))
  }
  final protected case class TargetPath(toPath: Path) extends SidedPath {
    override def toString: String = s"Target at $toPath"
  }
  protected object TargetPath {
    def apply(selector: Path => Path): TargetPath = new TargetPath(Path(selector))
  }

  sealed protected trait TransformerOverride extends scala.Product with Serializable
  protected object TransformerOverride {
    sealed trait ForField extends TransformerOverride
    sealed trait ForSubtype extends TransformerOverride
    sealed trait ForFallback extends TransformerOverride
    sealed trait ForConstructor extends TransformerOverride

    case object Unused extends ForField with ForSubtype {
      override def toString: String = "Unused"
    }

    final case class Const(runtimeData: ExistentialExpr) extends ForField {
      override def toString: String = s"Const(${ExistentialExpr.prettyPrint(runtimeData)})"
    }
    final case class ConstPartial(runtimeData: ExistentialExpr) extends ForField {
      override def toString: String = s"ConstPartial(${ExistentialExpr.prettyPrint(runtimeData)})"
    }

    final case class Computed(sourcePath: Path, targetPath: Path, runtimeData: ExistentialExpr)
        extends ForField
        with ForSubtype {
      override def toString: String = s"Computed($sourcePath, $targetPath, ${ExistentialExpr.prettyPrint(runtimeData)})"
    }
    final case class ComputedPartial(sourcePath: Path, targetPath: Path, runtimeData: ExistentialExpr)
        extends ForField
        with ForSubtype {
      override def toString: String =
        s"ComputedPartial($sourcePath, $targetPath, ${ExistentialExpr.prettyPrint(runtimeData)})"
    }

    final case class Fallback(runtimeData: ExistentialExpr) extends ForFallback {
      override def toString: String =
        s"Fallback(${ExistentialExpr.prettyPrint(runtimeData)})"
    }

    type Args = List[ListMap[String, ??]]
    final case class Constructor(args: Args, runtimeData: ExistentialExpr) extends ForConstructor {
      override def toString: String = s"Constructor(${printArgs(args)}, ${ExistentialExpr.prettyPrint(runtimeData)})"
    }
    final case class ConstructorPartial(args: Args, runtimeData: ExistentialExpr) extends ForConstructor {
      override def toString: String =
        s"ConstructorPartial(${printArgs(args)}, ${ExistentialExpr.prettyPrint(runtimeData)})"
    }

    final case class Renamed(sourcePath: Path, targetPath: Path) extends ForField with ForSubtype

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
      /** Stores all customizations provided by user at the top level of derivation */
      private val originalRuntimeOverrides: Vector[(SidedPath, TransformerOverride)] = Vector.empty,
      /** Let us prevent `implicit val foo = foo` but allow `implicit val foo = new Foo { def sth = foo }` */
      private val preventImplicitSummoningForTypes: Option[(??, ??)] = None
  ) {

    private lazy val runtimeOverridesForCurrent = runtimeOverrides.filter {
      case (SidedPath(Path.AtField(_, Path.Root)), _: TransformerOverride.ForField)     => true
      case (SidedPath(Path.AtSubtype(_, Path.Root)), _: TransformerOverride.ForSubtype) => true
      case (SidedPath(Path.Root), _: TransformerOverride.ForFallback)                   => true
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

    def addTransformerOverride(sidedPath: SidedPath, runtimeOverride: TransformerOverride): TransformerConfiguration = {
      val newRuntimeOverrides = runtimeOverrides :+ (sidedPath -> runtimeOverride)
      copy(runtimeOverrides = newRuntimeOverrides, originalRuntimeOverrides = newRuntimeOverrides)
    }
    def addTransformerOverride(
        sourcePath: Path,
        targetPath: Path,
        runtimeOverride: TransformerOverride
    ): TransformerConfiguration = {
      val newRuntimeOverrides =
        runtimeOverrides :+ (SourcePath(sourcePath) -> runtimeOverride) :+ (TargetPath(targetPath) -> runtimeOverride)
      copy(runtimeOverrides = newRuntimeOverrides, originalRuntimeOverrides = newRuntimeOverrides)
    }
    def areOverridesEmpty: Boolean =
      runtimeOverrides.view.filterNot(_._2.isInstanceOf[TransformerOverride.ForFallback]).isEmpty
    def areLocalFlagsAndOverridesEmpty: Boolean =
      areLocalFlagsEmpty && areOverridesEmpty

    def filterCurrentUnusedFields: Set[String] = ListSet.from(
      runtimeOverridesForCurrent.collect { case (SourcePath(Path.AtField(name, _)), TransformerOverride.Unused) =>
        name
      }
    )
    def filterCurrentUnusedSubtypes: Set[??] = ListSet.from(
      runtimeOverridesForCurrent.collect { case (TargetPath(Path.AtSubtype(tpe, _)), TransformerOverride.Unused) =>
        tpe
      }
    )
    def filterCurrentOverridesForField(nameFilter: String => Boolean): Map[String, TransformerOverride.ForField] =
      ListMap.from(
        runtimeOverridesForCurrent.collect {
          case (TargetPath(Path.AtField(name, _)), runtimeFieldOverride: TransformerOverride.ForField)
              if nameFilter(name) && runtimeFieldOverride != TransformerOverride.Unused =>
            name -> runtimeFieldOverride
        }
      )
    def filterCurrentOverridesForSubtype(
        sourceTypeFilter: ?? => Boolean
    ): Map[??, TransformerOverride.ForSubtype] = ListMap.from(
      runtimeOverridesForCurrent.collect {
        case (SourcePath(Path.AtSubtype(tpe, _)), runtimeCoproductOverride: TransformerOverride.ForSubtype)
            if sourceTypeFilter(tpe) && runtimeCoproductOverride != TransformerOverride.Unused =>
          tpe -> runtimeCoproductOverride
      }
    )
    def filterCurrentOverridesForSome: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (
              TargetPath(Path.AtSubtype(tpe, Path.AtField("value", Path.Root))),
              runtimeFieldOverride: TransformerOverride.ForField
            ) if tpe.Underlying <:< Type[Some[Any]] =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForLeft: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (
              TargetPath(Path.AtSubtype(tpe, Path.AtField("value", Path.Root))),
              runtimeFieldOverride: TransformerOverride.ForField
            ) if tpe.Underlying <:< Type[Left[Any, Any]] =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForRight: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (
              TargetPath(Path.AtSubtype(tpe, Path.AtField("value", Path.Root))),
              runtimeFieldOverride: TransformerOverride.ForField
            ) if tpe.Underlying <:< Type[Right[Any, Any]] =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForEveryItem: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (TargetPath(Path.AtItem(Path.Root)), runtimeFieldOverride: TransformerOverride.ForField) =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForEveryMapKey: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (TargetPath(Path.AtMapKey(Path.Root)), runtimeFieldOverride: TransformerOverride.ForField) =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForEveryMapValue: Set[TransformerOverride.ForField] = ListSet.from(
      runtimeOverrides.collect {
        case (TargetPath(Path.AtMapValue(Path.Root)), runtimeFieldOverride: TransformerOverride.ForField) =>
          runtimeFieldOverride
      }
    )
    def filterCurrentOverridesForFallbacks: Set[TransformerOverride.ForFallback] = ListSet.from(
      runtimeOverridesForCurrent.collect {
        case (SourcePath(_), runtimeConstructorOverride: TransformerOverride.ForFallback) =>
          runtimeConstructorOverride
      }
    )
    def currentOverrideForConstructor: Option[TransformerOverride.ForConstructor] =
      runtimeOverridesForCurrent.collectFirst {
        case (TargetPath(_), runtimeConstructorOverride: TransformerOverride.ForConstructor) =>
          runtimeConstructorOverride
      }

    def sourceFieldsUsedByOverrides(currentSrc: Path)(implicit ctx: TransformationContext[?, ?]): List[String] =
      originalRuntimeOverrides.view
        .collect {
          case (_, TransformerOverride.Computed(sourcePath, _, _))        => sourcePath.drop(currentSrc)
          case (_, TransformerOverride.ComputedPartial(sourcePath, _, _)) => sourcePath.drop(currentSrc)
          case (_, TransformerOverride.Renamed(sourcePath, _))            => sourcePath.drop(currentSrc)
        }
        .collect { case Some(Path.AtField(fromName, _)) => fromName }
        .toList
        .distinct
    def targetSubtypesUsedByOverrides(
        currentTgt: Path
    )(implicit ctx: TransformationContext[?, ?]): List[ExistentialType] =
      originalRuntimeOverrides.view
        .collect {
          case (_, TransformerOverride.Computed(_, targetPath, _))        => targetPath.drop(currentTgt)
          case (_, TransformerOverride.ComputedPartial(_, targetPath, _)) => targetPath.drop(currentTgt)
          case (_, TransformerOverride.Renamed(_, targetPath))            => targetPath.drop(currentTgt)
        }
        .collect { case Some(Path.AtSubtype(toSubtype, _)) => toSubtype }
        .toList
        .distinctBy(a => ExistentialType.prettyPrint(a))

    def prepareForRecursiveCall(
        fromPath: Path,
        toPath: Path,
        updateFallbacks: TransformerOverride.ForFallback => Vector[TransformerOverride.ForFallback]
    )(implicit ctx: TransformationContext[?, ?]): TransformerConfiguration =
      copy(
        flags = flags.prepareForRecursiveCall(fromPath, toPath),
        localFlagsOverridden = false,
        runtimeOverrides = runtimeOverrides.flatMap { case (sidedPath, runtimeOverride) =>
          runtimeOverride match {
            // Fields are always matched with "_.fieldName" Path while subtypes are always matched with
            // "_ match { case _: Tpe => }", so "_" Path (Root) is useless in their case while they might get in way of
            // checking if there might be some relevant overrides for current/nested values
            case _: TransformerOverride.ForField | _: TransformerOverride.ForSubtype =>
              val newSidedPath = sidedPath.drop(fromPath, toPath).view.filterNot(_.path == Path.Root)
              newSidedPath.map(_ -> runtimeOverride).filterNot(pathCannotBeUsedButBlocksRuleForEmptyOverrides)
            // Fallbacks are always matched at "_" Path, and dropped _manually_ only when going inward,
            // because we have to update their inner value
            case f: TransformerOverride.ForFallback =>
              if (sidedPath.path == Path.Root)
                Vector(sidedPath).view.flatMap(path => updateFallbacks(f).view.map(path -> _))
              else sidedPath.drop(fromPath, toPath).view.map(_ -> f)
            // Constructor is always matched at "_" Path, and dropped only when going inward
            case _: TransformerOverride.ForConstructor =>
              val newSidedPath = sidedPath.drop(fromPath, toPath).view
              newSidedPath.map(_ -> runtimeOverride)
          }
        },
        preventImplicitSummoningForTypes = None
      )

    // I haven't found a more "principled" way to achieve this, that wouldn't break half the tests
    private lazy val pathCannotBeUsedButBlocksRuleForEmptyOverrides: ((SidedPath, TransformerOverride)) => Boolean = {
      case (TargetPath(Path.AtSubtype(_, Path.Root)), _: TransformerOverride.Renamed) => true
      case _                                                                          => false
    }

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
    ](runtimeDataStore: Expr[PatcherDefinitionCommons.RuntimeDataStore]): TransformerConfiguration = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](TransformerFlags.global)
      val allFlags = extractTransformerFlags[InstanceFlags](implicitScopeFlags)
      val cfg = extractTransformerConfig[Tail](runtimeDataIdx = 0, runtimeDataStore).copy(flags = allFlags)
      if (wereLocalFlagsOverriden[InstanceFlags]) cfg.setLocalFlagsOverriden else cfg
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
          case ChimneyType.TransformerFlags.Flags.OptionFallbackMerge(s) =>
            if (s.Underlying =:= ChimneyType.SourceOrElseFallback)
              extractTransformerFlags[Flags2](defaultFlags).setOptionFallbackMerge(
                Some(dsls.SourceOrElseFallback)
              )
            else if (s.Underlying =:= ChimneyType.FallbackOrElseSource)
              extractTransformerFlags[Flags2](defaultFlags).setOptionFallbackMerge(
                Some(dsls.FallbackOrElseSource)
              )
            else {
              // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
              reportError("Invalid OptionFallbackMergeStrategy type!!")
              // $COVERAGE-ON$
            }
          case ChimneyType.TransformerFlags.Flags.EitherFallbackMerge(s) =>
            if (s.Underlying =:= ChimneyType.SourceOrElseFallback)
              extractTransformerFlags[Flags2](defaultFlags).setEitherFallbackMerge(
                Some(dsls.SourceOrElseFallback)
              )
            else if (s.Underlying =:= ChimneyType.FallbackOrElseSource)
              extractTransformerFlags[Flags2](defaultFlags).setEitherFallbackMerge(
                Some(dsls.FallbackOrElseSource)
              )
            else {
              // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
              reportError("Invalid OptionFallbackMergeStrategy type!!")
              // $COVERAGE-ON$
            }
          case ChimneyType.TransformerFlags.Flags.CollectionFallbackMerge(s) =>
            if (s.Underlying =:= ChimneyType.SourceAppendFallback)
              extractTransformerFlags[Flags2](defaultFlags).setCollectionFallbackMerge(
                Some(dsls.SourceAppendFallback)
              )
            else if (s.Underlying =:= ChimneyType.FallbackAppendSource)
              extractTransformerFlags[Flags2](defaultFlags).setCollectionFallbackMerge(
                Some(dsls.FallbackAppendSource)
              )
            else {
              // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
              reportError("Invalid CollectionFallbackMergeStrategy type!!")
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
          case ChimneyType.TransformerFlags.Flags.UnusedFieldPolicyCheck(p) =>
            if (p.Underlying =:= ChimneyType.FailOnIgnoredSourceVal) {
              extractTransformerFlags[Flags2](defaultFlags).setUnusedFieldPolicy(
                Some(dsls.FailOnIgnoredSourceVal)
              )
            } else {
              // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
              reportError("Invalid UnusedFieldPolicy type!!")
              // $COVERAGE-ON$
            }
          case ChimneyType.TransformerFlags.Flags.UnmatchedSubtypePolicyCheck(p) =>
            if (p.Underlying =:= ChimneyType.FailOnUnmatchedTargetSubtype) {
              extractTransformerFlags[Flags2](defaultFlags).setUnmatchedSubtypePolicy(
                Some(dsls.FailOnUnmatchedTargetSubtype)
              )
            } else {
              // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
              reportError("Invalid UnmatchedSubtypePolicy type!!")
              // $COVERAGE-ON$
            }
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
          case ChimneyType.TransformerFlags.Flags.OptionFallbackMerge(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setOptionFallbackMerge(None)
          case ChimneyType.TransformerFlags.Flags.EitherFallbackMerge(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setEitherFallbackMerge(None)
          case ChimneyType.TransformerFlags.Flags.CollectionFallbackMerge(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setCollectionFallbackMerge(None)
          case ChimneyType.TransformerFlags.Flags.FieldNameComparison(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setFieldNameComparison(None)
          case ChimneyType.TransformerFlags.Flags.SubtypeNameComparison(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setSubtypeNameComparison(None)
          case ChimneyType.TransformerFlags.Flags.UnusedFieldPolicyCheck(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setUnusedFieldPolicy(None)
          case ChimneyType.TransformerFlags.Flags.UnmatchedSubtypePolicyCheck(_) =>
            extractTransformerFlags[Flags2](defaultFlags).setUnmatchedSubtypePolicy(None)
          case _ =>
            extractTransformerFlags[Flags2](defaultFlags).setBoolFlag[Flag](value = false)
        }
      case ChimneyType.TransformerFlags.Source(sourcePath, sourceFlags, flags) =>
        import sourcePath.Underlying as FSourcePath, sourceFlags.Underlying as SourceFlags, flags.Underlying as Flags2
        extractTransformerFlags[Flags2](defaultFlags).setSourceFlags(extractPath[FSourcePath])(flags =>
          extractTransformerFlags[SourceFlags](flags)
        )
      case ChimneyType.TransformerFlags.Target(targetPath, targetFlags, flags) =>
        import targetPath.Underlying as FTargetPath, targetFlags.Underlying as TargetFlags, flags.Underlying as Flags2
        extractTransformerFlags[Flags2](defaultFlags).setTargetFlags(extractPath[FTargetPath])(flags =>
          extractTransformerFlags[TargetFlags](flags)
        )
      // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
      case _ =>
        reportError(s"Invalid internal TransformerFlags type shape: ${Type.prettyPrint[Flags]}!")
      // $COVERAGE-ON$
    }

    @scala.annotation.tailrec
    private def wereLocalFlagsOverriden[Flags <: runtime.TransformerFlags: Type]: Boolean = Type[Flags] match {
      case default if default =:= ChimneyType.TransformerFlags.Default => false
      case ChimneyType.TransformerFlags.Enable(flag, flags) =>
        import flag.Underlying as Flag, flags.Underlying as Flags2
        // ImplicitConversions and TypeConstraintEvidence are excluded from check, otherwise they would be impossible
        // to use with instance flags.
        if (Flag =:= ChimneyType.TransformerFlags.Flags.ImplicitConversions) wereLocalFlagsOverriden[Flags2]
        else if (Flag =:= ChimneyType.TransformerFlags.Flags.TypeConstraintEvidence) wereLocalFlagsOverriden[Flags2]
        // Whether or not we're logging macros should not affect the result of the derivation
        else if (Flag =:= ChimneyType.TransformerFlags.Flags.MacrosLogging) wereLocalFlagsOverriden[Flags2]
        else true
      case _ => true
    }

    private def extractTransformerConfig[Tail <: runtime.TransformerOverrides: Type](
        runtimeDataIdx: Int,
        runtimeDataStore: Expr[PatcherDefinitionCommons.RuntimeDataStore]
    ): TransformerConfiguration = Type[Tail] match {
      case empty if empty =:= ChimneyType.TransformerOverrides.Empty => TransformerConfiguration()
      case ChimneyType.TransformerOverrides.Unused(fromPath, cfg) =>
        import fromPath.Underlying as FromPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](runtimeDataIdx, runtimeDataStore).addTransformerOverride(
          SourcePath(extractPath[FromPath]),
          TransformerOverride.Unused
        )
      case ChimneyType.TransformerOverrides.Unmatched(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](runtimeDataIdx, runtimeDataStore).addTransformerOverride(
          TargetPath(extractPath[ToPath]),
          TransformerOverride.Unused
        )
      case ChimneyType.TransformerOverrides.Const(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore).addTransformerOverride(
          TargetPath(extractPath[ToPath]),
          TransformerOverride.Const(runtimeDataStore(runtimeDataIdx).as_??)
        )
      case ChimneyType.TransformerOverrides.ConstPartial(toPath, cfg) =>
        import toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore).addTransformerOverride(
          TargetPath(extractPath[ToPath]),
          TransformerOverride.ConstPartial(runtimeDataStore(runtimeDataIdx).as_??)
        )
      case ChimneyType.TransformerOverrides.Computed(fromPath, toPath, cfg) =>
        import fromPath.Underlying as FromPath, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        val sourcePath = extractPath[FromPath]
        val targetPath = extractPath[ToPath]
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore).addTransformerOverride(
          sourcePath,
          targetPath,
          TransformerOverride.Computed(sourcePath, targetPath, runtimeDataStore(runtimeDataIdx).as_??)
        )
      case ChimneyType.TransformerOverrides.ComputedPartial(fromPath, toPath, cfg) =>
        import fromPath.Underlying as FromPath, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        val sourcePath = extractPath[FromPath]
        val targetPath = extractPath[ToPath]
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore).addTransformerOverride(
          sourcePath,
          targetPath,
          TransformerOverride.ComputedPartial(sourcePath, targetPath, runtimeDataStore(runtimeDataIdx).as_??)
        )
      case ChimneyType.TransformerOverrides.Fallback(fallbackType, fromPath, cfg) =>
        import fallbackType.Underlying as FallbackType, fromPath.Underlying as FromPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore).addTransformerOverride(
          SourcePath(extractPath[FromPath]),
          TransformerOverride.Fallback(runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[FallbackType].as_??)
        )
      case ChimneyType.TransformerOverrides.Constructor(args, toPath, cfg) =>
        import args.Underlying as Args, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore).addTransformerOverride(
          TargetPath(extractPath[ToPath]),
          TransformerOverride.Constructor(extractArgumentLists[Args], runtimeDataStore(runtimeDataIdx).as_??)
        )
      case ChimneyType.TransformerOverrides.ConstructorPartial(args, toPath, cfg) =>
        import args.Underlying as Args, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        extractTransformerConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore).addTransformerOverride(
          TargetPath(extractPath[ToPath]),
          TransformerOverride.ConstructorPartial(extractArgumentLists[Args], runtimeDataStore(runtimeDataIdx).as_??)
        )
      case ChimneyType.TransformerOverrides.Renamed(fromPath, toPath, cfg) =>
        import fromPath.Underlying as FromPath, toPath.Underlying as ToPath, cfg.Underlying as Tail2
        val sourcePath = extractPath[FromPath]
        val targetPath = extractPath[ToPath]
        extractTransformerConfig[Tail2](runtimeDataIdx, runtimeDataStore).addTransformerOverride(
          sourcePath,
          targetPath,
          TransformerOverride.Renamed(sourcePath, targetPath)
        )
      // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
      case _ =>
        reportError(s"Invalid internal TransformerOverrides type shape: ${Type.prettyPrint[Tail]}!!")
      // $COVERAGE-ON$
    }

    // Exposed for Patcher Configuration
    def extractPath[PathType <: runtime.Path: Type]: Path = Type[PathType] match {
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

    private def extractNameComparisonObject[Comparison <: dsls.TransformedNamesComparison: Type]: Comparison =
      Type.extractObjectSingleton[Comparison].getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        reportError(
          s"Invalid TransformerNamesComparison type - only (case) objects are allowed, and only the ones defined as top-level or in top-level objects, got: ${Type
              .prettyPrint[Comparison]}!!!"
        )
        // $COVERAGE-ON$
      }
  }
}
