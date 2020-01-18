package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.internal.PatcherCfg
import io.scalaland.chimney.internal.PatcherCfg.EnableIncompletePatches

class PatcherInto[T, P, C <: PatcherCfg](val obj: T, val patchObject: P) {

  def enableIncompletePatches: PatcherInto[T, P, EnableIncompletePatches[C]] =
    new PatcherInto[T, P, EnableIncompletePatches[C]](obj, patchObject)

  def patch: T = obj

}
