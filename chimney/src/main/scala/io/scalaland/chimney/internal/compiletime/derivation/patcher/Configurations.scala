package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime

private[compiletime] trait Configurations { this: Derivation =>

  import Type.Implicits.*

  final protected case class PatcherFlags(
      ignoreNoneInPatch: Boolean = false,
      ignoreLeftInPatch: Boolean = false,
      appendCollectionInPatch: Boolean = false,
      ignoreRedundantPatcherFields: Boolean = false,
      displayMacrosLogging: Boolean = false,
      scopedUpdates: List[(SidedPath, PatcherFlags => PatcherFlags)] = List.empty
  ) {

    def setBoolFlag[Flag <: runtime.PatcherFlags.Flag: Type](value: Boolean): PatcherFlags =
      if (Type[Flag] =:= ChimneyType.PatcherFlags.Flags.IgnoreNoneInPatch) {
        copy(ignoreNoneInPatch = value)
      } else if (Type[Flag] =:= ChimneyType.PatcherFlags.Flags.IgnoreLeftInPatch) {
        copy(ignoreLeftInPatch = value)
      } else if (Type[Flag] =:= ChimneyType.PatcherFlags.Flags.AppendCollectionInPatch) {
        copy(appendCollectionInPatch = value)
      } else if (Type[Flag] =:= ChimneyType.PatcherFlags.Flags.IgnoreRedundantPatcherFields) {
        copy(ignoreRedundantPatcherFields = value)
      } else if (Type[Flag] =:= ChimneyType.PatcherFlags.Flags.MacrosLogging) {
        copy(displayMacrosLogging = value)
      } else {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        reportError(s"Invalid internal PatcherFlags type shape: ${Type.prettyPrint[Flag]}!")
        // $COVERAGE-ON$
      }

    def setPatchedValueFlags(objPath: Path)(update: PatcherFlags => PatcherFlags): PatcherFlags =
      copy(scopedUpdates = scopedUpdates :+ (TargetPath(objPath) -> update))

    lazy val toTransformerFlags: TransformerFlags = PatcherFlags.register(this) {
      TransformerFlags(
        optionFallbackMerge = if (ignoreNoneInPatch) Some(dsls.SourceOrElseFallback) else None,
        eitherFallbackMerge = if (ignoreLeftInPatch) Some(dsls.SourceOrElseFallback) else None,
        collectionFallbackMerge = if (appendCollectionInPatch) Some(dsls.FallbackAppendSource) else None,
        unusedFieldPolicy = if (ignoreRedundantPatcherFields) None else Some(dsls.FailOnIgnoredSourceVal),
        displayMacrosLogging = displayMacrosLogging,
        scopedUpdates = scopedUpdates.view.map { case (sidedPath, update) =>
          sidedPath -> ((tflags: TransformerFlags) => update(PatcherFlags.originalFlags(tflags)).toTransformerFlags)
        }.toList
      )
    }

    override def toString: String = s"PatcherFlags(${Vector(
        if (ignoreNoneInPatch) Vector("ignoreNoneInPatch") else Vector.empty,
        if (ignoreLeftInPatch) Vector("ignoreLeftInPatch") else Vector.empty,
        if (appendCollectionInPatch) Vector("appendCollectionInPatch") else Vector.empty,
        if (ignoreRedundantPatcherFields) Vector("ignoreRedundantPatcherFields") else Vector.empty,
        if (displayMacrosLogging) Vector("displayMacrosLogging") else Vector.empty
      ).flatten.mkString(", ")})"
  }
  protected object PatcherFlags {

    /** Let us implement TransformerFlags => TransformerFlags using PatcherFlags => PatcherFlags */
    private val originalFlags = scala.collection.mutable.Map.empty[TransformerFlags, PatcherFlags]
    def register(patchFlags: PatcherFlags)(transformerFlags: TransformerFlags): TransformerFlags = {
      originalFlags += (transformerFlags -> patchFlags)
      transformerFlags
    }

    // $COVERAGE-OFF$It's testable in (Scala-CLI) snippets and not really in normal tests with coverage
    def global: PatcherFlags = XMacroSettings.foldLeft(PatcherFlags()) {
      case (cfg, patcherFlag"IgnoreNoneInPatch=$value")       => cfg.copy(ignoreNoneInPatch = value.toBoolean)
      case (cfg, patcherFlag"IgnoreLeftInPatch=$value")       => cfg.copy(ignoreLeftInPatch = value.toBoolean)
      case (cfg, patcherFlag"AppendCollectionInPatch=$value") => cfg.copy(appendCollectionInPatch = value.toBoolean)
      case (cfg, patcherFlag"IgnoreRedundantPatcherFields=$value") =>
        cfg.copy(ignoreRedundantPatcherFields = value.toBoolean)
      case (cfg, patcherFlag"MacrosLogging=$value") => cfg.copy(displayMacrosLogging = value.toBoolean)
      case (cfg, _)                                 => cfg
    }
    // $COVERAGE-ON$
  }

  sealed protected trait PatcherOverride extends scala.Product with Serializable
  protected object PatcherOverride {
    case object Ignored extends PatcherOverride {
      override def toString: String = "Ignored"
    }

    final case class Const(runtimeData: ExistentialExpr) extends PatcherOverride {
      override def toString: String = s"Const(${ExistentialExpr.prettyPrint(runtimeData)})"
    }

    final case class Computed(sourcePath: Path, targetPath: Path, runtimeData: ExistentialExpr)
        extends PatcherOverride {
      override def toString: String = s"Computed($sourcePath, $targetPath, ${ExistentialExpr.prettyPrint(runtimeData)})"
    }
  }

  final protected case class PatcherConfiguration(
      flags: PatcherFlags = PatcherFlags(),
      /** Let us distinct if flags were modified only by implicit TransformerConfiguration or maybe also locally */
      private val localFlagsOverridden: Boolean = false,
      /** Stores all customizations provided by user */
      private val runtimeOverrides: Vector[(SidedPath, PatcherOverride)] = Vector.empty,
      /** Let us prevent `implicit val foo = foo` but allow `implicit val foo = new Foo { def sth = foo }` */
      private val preventImplicitSummoningForTypes: Option[(??, ??)] = None
  ) {

    def allowAPatchImplicitSearch: PatcherConfiguration = copy(preventImplicitSummoningForTypes = None)
    def preventImplicitSummoningFor[A: Type, Patch: Type]: PatcherConfiguration =
      copy(preventImplicitSummoningForTypes = Some(Type[A].as_?? -> Type[Patch].as_??))
    def isImplicitSummoningPreventedFor[A: Type, Patch: Type]: Boolean =
      preventImplicitSummoningForTypes.exists { case (someA, somePatch) =>
        import someA.Underlying as SomeA, somePatch.Underlying as SomePatch
        Type[SomeA] =:= Type[A] && Type[SomePatch] =:= Type[Patch]
      }

    def setLocalFlagsOverriden: PatcherConfiguration =
      copy(localFlagsOverridden = true)

    def addPatcherOverride(sidedPath: SidedPath, runtimeOverride: PatcherOverride): PatcherConfiguration = {
      val newRuntimeOverrides = runtimeOverrides :+ (sidedPath -> runtimeOverride)
      copy(runtimeOverrides = newRuntimeOverrides)
    }
    def addPatcherOverride(
        sourcePath: Path,
        targetPath: Path,
        runtimeOverride: PatcherOverride
    ): PatcherConfiguration = {
      val newRuntimeOverrides =
        runtimeOverrides :+ (SourcePath(sourcePath) -> runtimeOverride) :+ (TargetPath(targetPath) -> runtimeOverride)
      copy(runtimeOverrides = newRuntimeOverrides)
    }

    def toTransformerConfiguration(obj: ExistentialExpr): TransformerConfiguration = {
      val transformerOverrides =
        (SourcePath(Path.Root) -> (TransformerOverride.Fallback(obj): TransformerOverride)) +: runtimeOverrides
          .map {
            case (sidedPatch, PatcherOverride.Ignored) =>
              sidedPatch -> (TransformerOverride.Unused: TransformerOverride)
            case (sidedPath, PatcherOverride.Const(expr)) =>
              sidedPath -> (TransformerOverride.Const(expr): TransformerOverride)
            case (sidedPath, PatcherOverride.Computed(sourcePath, targetPath, expr)) =>
              sidedPath -> (TransformerOverride.Computed(sourcePath, targetPath, expr): TransformerOverride)
          }
      TransformerConfiguration(
        flags = flags.toTransformerFlags,
        localFlagsOverridden = localFlagsOverridden,
        runtimeOverrides = transformerOverrides,
        originalRuntimeOverrides = transformerOverrides,
        preventImplicitSummoningForTypes = preventImplicitSummoningForTypes.map(_.swap)
      )
    }

    override def toString: String = {
      val preventImplicitSummoningForTypesString = preventImplicitSummoningForTypes.map { case (f, t) =>
        s"(${ExistentialType.prettyPrint(f)}, ${ExistentialType.prettyPrint(t)})"
      }.toString
      s"""PatcherConfig(
         |  flags = $flags,
         |  preventImplicitSummoningForTypes = $preventImplicitSummoningForTypesString
         |)""".stripMargin
    }
  }

  protected object PatcherConfigurations {

    final def readPatcherConfiguration[
        Tail <: runtime.PatcherOverrides: Type,
        InstanceFlags <: runtime.PatcherFlags: Type,
        ImplicitScopeFlags <: runtime.PatcherFlags: Type
    ](runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]): PatcherConfiguration = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](PatcherFlags.global)
      val allFlags = extractTransformerFlags[InstanceFlags](implicitScopeFlags)
      val cfg = extractPatcherConfig[Tail](runtimeDataIdx = 0, runtimeDataStore).copy(flags = allFlags)
      if (wereLocalFlagsOverriden[InstanceFlags]) cfg.setLocalFlagsOverriden else cfg
    }

    import TransformerConfigurations.extractPath

    private def extractTransformerFlags[Flags <: runtime.PatcherFlags: Type](defaultFlags: PatcherFlags): PatcherFlags =
      Type[Flags] match {
        case default if default =:= ChimneyType.PatcherFlags.Default => defaultFlags
        case ChimneyType.PatcherFlags.Enable(flag, flags) =>
          import flag.Underlying as Flag, flags.Underlying as Flags2
          extractTransformerFlags[Flags2](defaultFlags).setBoolFlag[Flag](value = true)
        case ChimneyType.PatcherFlags.Disable(flag, flags) =>
          import flag.Underlying as Flag, flags.Underlying as Flags2
          extractTransformerFlags[Flags2](defaultFlags).setBoolFlag[Flag](value = false)
        case ChimneyType.PatcherFlags.PatchedValue(objPath, objFlags, flags) =>
          import objPath.Underlying as ObjPath, objFlags.Underlying as ObjFlags, flags.Underlying as Flags2
          extractTransformerFlags[Flags2](defaultFlags).setPatchedValueFlags(extractPath[ObjPath])(flags =>
            extractTransformerFlags[ObjFlags](flags)
          )
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        case _ =>
          reportError(s"Invalid internal PatcherFlags type shape: ${Type.prettyPrint[Flags]}!")
        // $COVERAGE-ON$
      }

    @scala.annotation.tailrec
    private def wereLocalFlagsOverriden[Flags <: runtime.PatcherFlags: Type]: Boolean = Type[Flags] match {
      case default if default =:= ChimneyType.PatcherFlags.Default => false
      case ChimneyType.PatcherFlags.Enable(flag, flags) =>
        import flag.Underlying as Flag, flags.Underlying as Flags2
        // Whether or not we're logging macros should not affect the result of the derivation
        if (Flag =:= ChimneyType.PatcherFlags.Flags.MacrosLogging) wereLocalFlagsOverriden[Flags2]
        else true
      case _ => true
    }

    private def extractPatcherConfig[Tail <: runtime.PatcherOverrides: Type](
        runtimeDataIdx: Int,
        runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
    ): PatcherConfiguration =
      Type[Tail] match {
        case empty if empty =:= ChimneyType.PatcherOverrides.Empty => PatcherConfiguration()
        case ChimneyType.PatcherOverrides.Ignored(patchPath, cfg) =>
          import patchPath.Underlying as PatchPath, cfg.Underlying as Tail2
          extractPatcherConfig[Tail2](runtimeDataIdx, runtimeDataStore).addPatcherOverride(
            SourcePath(extractPath[PatchPath]),
            PatcherOverride.Ignored
          )
        case ChimneyType.PatcherOverrides.Const(objPath, cfg) =>
          import objPath.Underlying as ObjPath, cfg.Underlying as Tail2
          extractPatcherConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore).addPatcherOverride(
            TargetPath(extractPath[ObjPath]),
            PatcherOverride.Const(runtimeDataStore(runtimeDataIdx).as_??)
          )
        case ChimneyType.PatcherOverrides.Computed(patchPath, objPath, cfg) =>
          import patchPath.Underlying as PatchPath, objPath.Underlying as ObjPath, cfg.Underlying as Tail2
          val sourcePath = extractPath[PatchPath]
          val targetPath = extractPath[ObjPath]
          extractPatcherConfig[Tail2](1 + runtimeDataIdx, runtimeDataStore).addPatcherOverride(
            sourcePath,
            targetPath,
            PatcherOverride.Computed(sourcePath, targetPath, runtimeDataStore(runtimeDataIdx).as_??)
          )
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        case _ =>
          reportError(s"Invalid internal PatcherOverrides type shape: ${Type.prettyPrint[Tail]}!!")
        // $COVERAGE-ON$
      }
  }
}
