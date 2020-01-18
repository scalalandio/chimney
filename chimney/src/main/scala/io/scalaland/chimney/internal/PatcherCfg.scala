package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

sealed abstract class PatcherCfg

object PatcherCfg {
  final class Empty extends PatcherCfg
  final class IgnoreRedundantPatcherFields[C <: PatcherCfg] extends PatcherCfg
}

trait PatcherConfiguration {

  val c: blackbox.Context

  import c.universe._

  case class PatcherConfig(
      ignoreRedundantPatcherFields: Boolean = false
  )

  def capturePatcherConfig(cfgTpe: Type, config: PatcherConfig = PatcherConfig()): PatcherConfig = {

    import PatcherCfg._

    val emptyT = typeOf[Empty]
    val ignoreRedundantPatcherFields = typeOf[IgnoreRedundantPatcherFields[_]].typeConstructor

    if (cfgTpe =:= emptyT) {
      config
    } else if (cfgTpe.typeConstructor =:= ignoreRedundantPatcherFields) {
      capturePatcherConfig(cfgTpe.typeArgs.head, config.copy(ignoreRedundantPatcherFields = true))
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal patcher config type shape!")
      // $COVERAGE-ON$
    }
  }
}
