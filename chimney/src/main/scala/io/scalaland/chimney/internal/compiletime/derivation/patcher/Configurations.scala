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
        reportError(s"Invalid patcher flag type: ${Type[Flag]}!")
        // $COVERAGE-ON$
      }
  }

  final protected case class PatcherConfig(
      flags: PatcherFlags = PatcherFlags()
  )

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

    // This (suppressed) error is a case when compiler is simply wrong :)
    @scala.annotation.nowarn("msg=Unreachable case")
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
          reportError(s"Bad internal transformer flags type shape ${Type.prettyPrint[Flags]}!")
        // $COVERAGE-ON$
      }

    // This (suppressed) error is a case when compiler is simply wrong :)
    @scala.annotation.nowarn("msg=Unreachable case")
    private def extractPatcherConfig[Cfg <: runtime.PatcherCfg: Type](): PatcherConfig = Type[Cfg] match {
      case empty if empty =:= ChimneyType.PatcherCfg.Empty => PatcherConfig()
      case _ =>
        reportError(s"Bad internal patcher config type shape ${Type.prettyPrint[Cfg]}!!")
    }
  }
}
