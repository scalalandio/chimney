package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.runtime

private[compiletime] trait Configurations { this: Derivation =>

  final protected case class PatcherFlags(
      ignoreNoneInPatch: Boolean = false,
      ignoreRedundantPatcherFields: Boolean = false,
      displayMacrosLogging: Boolean = false
  ) {

    def setBoolFlag[Flag <: runtime.PatcherFlags.Flag: Type](value: Boolean): PatcherFlags =
      if (Type[Flag] =:= ChimneyType.PatcherFlags.Flags.IgnoreNoneInPatch) {
        copy(ignoreNoneInPatch = value)
      } else if (Type[Flag] =:= ChimneyType.PatcherFlags.Flags.IgnoreRedundantPatcherFields) {
        copy(ignoreRedundantPatcherFields = value)
      } else if (Type[Flag] =:= ChimneyType.PatcherFlags.Flags.MacrosLogging) {
        copy(displayMacrosLogging = value)
      } else {
        // $COVERAGE-OFF$
        reportError(s"Invalid internal PatcherFlags type shape: ${Type[Flag]}!")
        // $COVERAGE-ON$
      }

    override def toString: String = s"PatcherFlags(${Vector(
        if (ignoreNoneInPatch) Vector("ignoreNoneInPatch") else Vector.empty,
        if (ignoreRedundantPatcherFields) Vector("ignoreRedundantPatcherFields") else Vector.empty,
        if (displayMacrosLogging) Vector("displayMacrosLogging") else Vector.empty
      ).flatten.mkString(", ")})"
  }

  final protected case class PatcherConfig(
      flags: PatcherFlags = PatcherFlags(),
      private val preventImplicitSummoningForTypes: Option[(??, ??)] = None
  ) {

    def allowAPatchImplicitSearch: PatcherConfig = copy(preventImplicitSummoningForTypes = None)

    def preventImplicitSummoningFor[A: Type, Patch: Type]: PatcherConfig =
      copy(preventImplicitSummoningForTypes = Some(Type[A].as_?? -> Type[Patch].as_??))

    def isImplicitSummoningPreventedFor[A: Type, Patch: Type]: Boolean =
      preventImplicitSummoningForTypes.exists { case (someA, somePatch) =>
        import someA.Underlying as SomeA, somePatch.Underlying as SomePatch
        Type[SomeA] =:= Type[A] && Type[SomePatch] =:= Type[Patch]
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

    final def readPatcherConfig[
        Cfg <: runtime.PatcherCfg: Type,
        Flags <: runtime.PatcherFlags: Type,
        ImplicitScopeFlags <: runtime.PatcherFlags: Type
    ]: PatcherConfig = {
      val implicitScopeFlags = extractTransformerFlags[ImplicitScopeFlags](PatcherFlags())
      val allFlags = extractTransformerFlags[Flags](implicitScopeFlags)
      extractPatcherConfig[Cfg]().copy(flags = allFlags)
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
        case _ =>
          // $COVERAGE-OFF$
          reportError(s"Invalid internal PatcherFlags type shape: ${Type.prettyPrint[Flags]}!")
        // $COVERAGE-ON$
      }

    private def extractPatcherConfig[Cfg <: runtime.PatcherCfg: Type](): PatcherConfig = Type[Cfg] match {
      case empty if empty =:= ChimneyType.PatcherCfg.Empty => PatcherConfig()
      case _ =>
        reportError(s"Invalid internal PatcherCfg type shape: ${Type.prettyPrint[Cfg]}!!")
    }
  }
}
