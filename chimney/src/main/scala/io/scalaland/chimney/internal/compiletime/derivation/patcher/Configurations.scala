package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime

private[compiletime] trait Configurations { this: Derivation =>

  final protected case class PatcherFlags(
      ignoreNoneInPatch: Boolean = false,
      ignoreLeftInPatch: Boolean = false,
      appendCollectionInPatch: Boolean = false,
      ignoreRedundantPatcherFields: Boolean = false,
      displayMacrosLogging: Boolean = false
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
        reportError(s"Invalid internal PatcherFlags type shape: ${Type[Flag]}!")
        // $COVERAGE-ON$
      }

    def toTransformerFlags: TransformerFlags = TransformerFlags(
      optionFallbackMerge = if (ignoreNoneInPatch) Some(dsls.SourceOrElseFallback) else None,
      eitherFallbackMerge = if (ignoreLeftInPatch) Some(dsls.SourceOrElseFallback) else None,
      collectionFallbackMerge = if (appendCollectionInPatch) Some(dsls.FallbackAppendSource) else None,
      unusedFieldPolicy = if (ignoreRedundantPatcherFields) None else Some(dsls.FailOnIgnoredSourceVal),
      displayMacrosLogging = displayMacrosLogging
    )

    override def toString: String = s"PatcherFlags(${Vector(
        if (ignoreNoneInPatch) Vector("ignoreNoneInPatch") else Vector.empty,
        if (ignoreLeftInPatch) Vector("ignoreLeftInPatch") else Vector.empty,
        if (appendCollectionInPatch) Vector("appendCollectionInPatch") else Vector.empty,
        if (ignoreRedundantPatcherFields) Vector("ignoreRedundantPatcherFields") else Vector.empty,
        if (displayMacrosLogging) Vector("displayMacrosLogging") else Vector.empty
      ).flatten.mkString(", ")})"
  }
  protected object PatcherFlags {

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

    def toTransformerConfiguration(obj: ExistentialExpr): TransformerConfiguration = {
      val transformerOverrides =
        (SourcePath(Path.Root) -> (TransformerOverride.Fallback(obj): TransformerOverride)) +: runtimeOverrides
          .map {
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
    ]: PatcherConfiguration = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](PatcherFlags.global)
      val allFlags = extractTransformerFlags[InstanceFlags](implicitScopeFlags)
      val cfg = extractPatcherConfig[Tail]().copy(flags = allFlags)
      if (Type[InstanceFlags] =:= ChimneyType.PatcherFlags.Default) cfg else cfg.setLocalFlagsOverriden
    }

    private def extractTransformerFlags[Flags <: runtime.PatcherFlags: Type](defaultFlags: PatcherFlags): PatcherFlags =
      Type[Flags] match {
        case default if default =:= ChimneyType.PatcherFlags.Default => defaultFlags
        case ChimneyType.PatcherFlags.Enable(flag, flags) =>
          import flag.Underlying as Flag, flags.Underlying as Flags2
          extractTransformerFlags[Flags2](defaultFlags).setBoolFlag[Flag](value = true)
        case ChimneyType.PatcherFlags.Disable(flag, flags) =>
          import flag.Underlying as Flag, flags.Underlying as Flags2
          extractTransformerFlags[Flags2](defaultFlags).setBoolFlag[Flag](value = false)
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        case _ =>
          reportError(s"Invalid internal PatcherFlags type shape: ${Type.prettyPrint[Flags]}!")
        // $COVERAGE-ON$
      }

    private def extractPatcherConfig[Tail <: runtime.PatcherOverrides: Type](): PatcherConfiguration =
      Type[Tail] match {
        case empty if empty =:= ChimneyType.PatcherOverrides.Empty => PatcherConfiguration()
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        case _ =>
          reportError(s"Invalid internal PatcherOverrides type shape: ${Type.prettyPrint[Tail]}!!")
        // $COVERAGE-ON$
      }
  }
}
