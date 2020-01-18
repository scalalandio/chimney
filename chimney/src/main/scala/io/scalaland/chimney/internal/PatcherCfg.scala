package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

sealed abstract class PatcherCfg

object PatcherCfg {
  final class Empty extends PatcherCfg
  final class EnableIncompletePatches[C <: PatcherCfg] extends PatcherCfg
}

trait PatcherConfiguration {

  val c: blackbox.Context

  import c.universe._

  case class PatcherConfig(
      enableIncompletePatches: Boolean = false
  )

  def capturePatcherConfig(cfgTpe: Type, config: PatcherConfig = PatcherConfig()): PatcherConfig = {

    import PatcherCfg._

    val emptyT = typeOf[Empty]
    val enableIncompletePatches = typeOf[EnableIncompletePatches[_]].typeConstructor

    if (cfgTpe =:= emptyT) {
      config
    } else if (cfgTpe.typeConstructor =:= enableIncompletePatches) {
      capturePatcherConfig(cfgTpe.typeArgs.head, config.copy(enableIncompletePatches = true))
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal patcher config type shape!")
      // $COVERAGE-ON$
    }
  }
}
