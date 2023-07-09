package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.runtime

private[compiletime] trait Configurations { this: Derivation =>

  final protected case class PatcherConfig(
      ignoreNoneInPatch: Boolean = false,
      ignoreRedundantPatcherFields: Boolean = false,
      displayMacrosLogging: Boolean = false
  )

  protected object PatcherConfigurations {

    final def readPatcherConfig[Cfg <: runtime.PatcherCfg: Type]: PatcherConfig =
      readPatcherConfigAux(PatcherConfig())

    // This (suppressed) error is a case when Scala 3 compiler is simply wrong :)
    @scala.annotation.nowarn("msg=Unreachable case")
    private def readPatcherConfigAux[Cfg <: runtime.PatcherCfg: Type](cfg: PatcherConfig): PatcherConfig =
      Type[Cfg] match {
        case empty if empty =:= ChimneyType.PatcherCfg.Empty => cfg
        case ChimneyType.PatcherCfg.IgnoreRedundantPatcherFields(cfgRest) =>
          import cfgRest.Underlying as CfgRest
          readPatcherConfigAux[cfgRest.Underlying](cfg).copy(ignoreRedundantPatcherFields = true)
        case ChimneyType.PatcherCfg.IgnoreNoneInPatch(cfgRest) =>
          import cfgRest.Underlying as CfgRest
          readPatcherConfigAux[cfgRest.Underlying](cfg).copy(ignoreNoneInPatch = true)
        case ChimneyType.PatcherCfg.MacrosLogging(cfgRest) =>
          import cfgRest.Underlying as CfgRest
          readPatcherConfigAux[cfgRest.Underlying](cfg).copy(displayMacrosLogging = true)
        case _ =>
          // $COVERAGE-OFF$
          reportError(s"Bad internal patcher config type shape ${Type.prettyPrint[Cfg]}!")
        // $COVERAGE-ON$
      }
  }
}
