package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides}

import scala.language.experimental.macros

/** Provides operations to customize [[io.scalaland.chimney.Patcher]] logic for specific
  * object value and patch value.
  *
  * @tparam A       type of object to apply patch to
  * @tparam Patch   type of patch object
  * @tparam Cfg     type-level encoded config
  * @tparam Flags   type-level encoded flags
  * @param obj      object to patch
  * @param objPatch patch object
  *
  * @since 0.4.0
  */
final class PatcherUsing[A, Patch, Cfg <: PatcherOverrides, Flags <: PatcherFlags](val obj: A, val objPatch: Patch)
    extends PatcherFlagsDsl[Lambda[`Flags1 <: PatcherFlags` => PatcherUsing[A, Patch, Cfg, Flags1]], Flags] {

  /** Applies configured patching in-place
    *
    * @return patched value
    *
    * @since 0.4.0
    */
  def patch[ImplicitScopeFlags <: PatcherFlags](implicit
      tc: PatcherConfiguration[ImplicitScopeFlags]
  ): A = macro PatcherMacros.derivePatchWithConfig[A, Patch, Cfg, Flags, ImplicitScopeFlags]
}
