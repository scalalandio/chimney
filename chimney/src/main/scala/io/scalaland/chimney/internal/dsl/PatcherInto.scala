package io.scalaland.chimney.internal.dsl

import io.scalaland.chimney.internal.PatcherCfg
import io.scalaland.chimney.internal.PatcherCfg.{IgnoreNoneInPatch, IgnoreRedundantPatcherFields}
import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros

class PatcherInto[T, P, C <: PatcherCfg](val obj: T, val objPatch: P) {

  def ignoreNoneInPatch: PatcherInto[T, P, IgnoreNoneInPatch[C]] =
    this.asInstanceOf[PatcherInto[T, P, IgnoreNoneInPatch[C]]]

  def ignoreRedundantPatcherFields: PatcherInto[T, P, IgnoreRedundantPatcherFields[C]] =
    this.asInstanceOf[PatcherInto[T, P, IgnoreRedundantPatcherFields[C]]]

  def patch: T = macro ChimneyBlackboxMacros.patchImpl[T, P, C]

}
