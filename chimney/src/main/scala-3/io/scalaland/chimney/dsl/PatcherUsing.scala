package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.PatcherCfg.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.runtime.PatcherCfg

final class PatcherUsing[A, Patch, Cfg <: PatcherCfg](val obj: A, val objPatch: Patch) {

  def ignoreNoneInPatch: PatcherUsing[A, Patch, IgnoreNoneInPatch[Cfg]] =
    this.asInstanceOf[PatcherUsing[A, Patch, IgnoreNoneInPatch[Cfg]]]

  def ignoreRedundantPatcherFields: PatcherUsing[A, Patch, IgnoreRedundantPatcherFields[Cfg]] =
    this.asInstanceOf[PatcherUsing[A, Patch, IgnoreRedundantPatcherFields[Cfg]]]

  def enableMacrosLogging: PatcherUsing[A, Patch, MacrosLogging[Cfg]] =
    this.asInstanceOf[PatcherUsing[A, Patch, MacrosLogging[Cfg]]]

  inline def patch: A = ${ PatcherMacros.derivePatcherResult[A, Patch, Cfg]('obj, 'objPatch) }
}
