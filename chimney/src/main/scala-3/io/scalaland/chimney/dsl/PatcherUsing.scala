package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.PatcherCfg
import io.scalaland.chimney.internal.PatcherCfg.*

final class PatcherUsing[T, P, Cfg <: PatcherCfg](val obj: T, val objPatch: P) {

  def ignoreNoneInPatch: PatcherUsing[T, P, IgnoreNoneInPatch[Cfg]] =
    this.asInstanceOf[PatcherUsing[T, P, IgnoreNoneInPatch[Cfg]]]

  def ignoreRedundantPatcherFields: PatcherUsing[T, P, IgnoreRedundantPatcherFields[Cfg]] =
    this.asInstanceOf[PatcherUsing[T, P, IgnoreRedundantPatcherFields[Cfg]]]

  def patch: T = ??? // TODO: impl
}
