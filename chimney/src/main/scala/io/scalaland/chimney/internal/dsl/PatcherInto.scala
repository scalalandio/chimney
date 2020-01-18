package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.internal.PatcherCfg
import io.scalaland.chimney.internal.PatcherCfg.EnableIncompletePatches
import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros

class PatcherInto[T, P, C <: PatcherCfg](val obj: T, val objPatch: P) {

  def enableIncompletePatches: PatcherInto[T, P, EnableIncompletePatches[C]] =
    new PatcherInto[T, P, EnableIncompletePatches[C]](obj, objPatch)

  def patch: T = macro ChimneyBlackboxMacros.patchImpl[T, P, C]

}
