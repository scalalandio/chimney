package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.PatcherCfg
import io.scalaland.chimney.internal.PatcherCfg.*
import io.scalaland.chimney.internal.macros.dsl.PatcherBlackboxMacros

import scala.language.experimental.macros

/** Provides operations to customize patcher logic for specific
  * object value and patch value.
  *
  * @tparam T       type of object to apply patch to
  * @tparam P       type of patch object
  * @tparam Cfg     type-level encoded configuration of patcher
  * @param obj      object to patch
  * @param objPatch patch object
  *
  * @since 0.4.0
  */
final class PatcherUsing[T, P, Cfg <: PatcherCfg](val obj: T, val objPatch: P) {

  /** In case when both object to patch and patch value contain field
    * of type `Option[T]`, this option allows to treat `None` value in
    * patch like the value was not provided.
    *
    * By default, when `None` is delivered in patch, Chimney clears
    * the value of such field on patching.
    *
    * @see [[https://scalalandio.github.io/chimney/patchers/options-handling.html]] for more details
    *
    * @return [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.4.0
    */
  def ignoreNoneInPatch: PatcherUsing[T, P, IgnoreNoneInPatch[Cfg]] =
    this.asInstanceOf[PatcherUsing[T, P, IgnoreNoneInPatch[Cfg]]]

  /** In case that patch object contains a redundant field (i.e. field that
    * is not present in patched object type), this option enables ignoring
    * value of such fields and generate patch successfully.
    *
    * By default, when Chimney detects a redundant field in patch object, it
    * fails the compilation in order to prevent silent oversight of field name
    * typos.
    *
    * @see [[https://scalalandio.github.io/chimney/patchers/redundant-fields.html]] for more details
    *
    * @return [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.4.0
    */
  def ignoreRedundantPatcherFields: PatcherUsing[T, P, IgnoreRedundantPatcherFields[Cfg]] =
    this.asInstanceOf[PatcherUsing[T, P, IgnoreRedundantPatcherFields[Cfg]]]

  def enableMacrosLogging: PatcherUsing[T, P, MacrosLogging[Cfg]] =
    this.asInstanceOf[PatcherUsing[T, P, MacrosLogging[Cfg]]]

  /** Applies configured patching in-place
    *
    * @return patched value
    *
    * @since 0.4.0
    */
  def patch: T = macro PatcherBlackboxMacros.patchImpl[T, P, Cfg]
}
