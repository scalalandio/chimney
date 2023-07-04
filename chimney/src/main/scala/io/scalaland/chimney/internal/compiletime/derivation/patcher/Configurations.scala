package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal

private[compiletime] trait Configurations { this: Derivation =>

  case class PatcherConfig(
      ignoreNoneInPatch: Boolean = false,
      ignoreRedundantPatcherFields: Boolean = false,
      displayMacrosLogging: Boolean = false
  )

  protected object PatcherConfigurations {

    final def readPatcherConfig[Cfg <: internal.PatcherCfg: Type]: PatcherConfig = {
      readPatcherConfigAux(PatcherConfig())
    }

    private def readPatcherConfigAux[Cfg <: internal.PatcherCfg: Type](cfg: PatcherConfig): PatcherConfig = Type[Cfg] match    {
      case empty if empty =:= ChimneyType.PatcherCfg.Empty => cfg
      case ChimneyType.PatcherCfg.IgnoreRedundantPatcherFields(cfgRest) =>
        ExistentialType.Bounded.use(cfgRest) {
          implicit CfgRest: Type[cfgRest.Underlying] =>
            readPatcherConfigAux[cfgRest.Underlying](cfg).copy(ignoreRedundantPatcherFields = true)
        }
      case ChimneyType.PatcherCfg.IgnoreNoneInPatch(cfgRest) =>
        ExistentialType.Bounded.use(cfgRest) {
          implicit CfgRest: Type[cfgRest.Underlying] =>
            readPatcherConfigAux[cfgRest.Underlying](cfg).copy(ignoreNoneInPatch = true)
        }
      case ChimneyType.PatcherCfg.MacrosLogging(cfgRest) =>
        ExistentialType.Bounded.use(cfgRest) {
          implicit CfgRest: Type[cfgRest.Underlying] =>
            readPatcherConfigAux[cfgRest.Underlying](cfg).copy(displayMacrosLogging = true)
        }
      case _ =>
        // $COVERAGE-OFF$
        reportError(s"Bad internal patcher config type shape ${Type.prettyPrint[Cfg]}!")
        // $COVERAGE-ON$
    }
  }
}
