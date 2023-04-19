package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait PatcherConfiguration {

  val c: blackbox.Context

  import c.universe.*

  case class PatcherConfig(
      ignoreNoneInPatch: Boolean = false,
      ignoreRedundantPatcherFields: Boolean = false
  )

  def capturePatcherConfig(cfgTpe: Type, config: PatcherConfig = PatcherConfig()): PatcherConfig = {

    import PatcherCfg.*

    val emptyT = typeOf[Empty]
    val ignoreRedundantPatcherFields = typeOf[IgnoreRedundantPatcherFields[?]].typeConstructor
    val ignoreNoneInPatch = typeOf[IgnoreNoneInPatch[?]].typeConstructor

    if (cfgTpe =:= emptyT) {
      config
    } else if (cfgTpe.typeConstructor =:= ignoreRedundantPatcherFields) {
      capturePatcherConfig(cfgTpe.typeArgs.head, config.copy(ignoreRedundantPatcherFields = true))
    } else if (cfgTpe.typeConstructor =:= ignoreNoneInPatch) {
      capturePatcherConfig(cfgTpe.typeArgs.head, config.copy(ignoreNoneInPatch = true))
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal patcher config type shape!")
      // $COVERAGE-ON$
    }
  }
}
