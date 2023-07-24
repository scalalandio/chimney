package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.PatcherCfg.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.runtime.PatcherCfg

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
final class PatcherUsing[A, Patch, Cfg <: PatcherCfg](val obj: A, val objPatch: Patch) {

  /** In case when both object to patch and patch value contain field
    * of type `Option[T]`, this option allows to treat `None` value in
    * patch like the value was not provided.
    *
    * By default, when `None` is delivered in patch, Chimney clears
    * the value of such field on patching.
    *
    * @see [[https://chimney.readthedocs.io/patchers/options-handling.html]] for more details
    * @return [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.4.0
    */
  def ignoreNoneInPatch: PatcherUsing[A, Patch, IgnoreNoneInPatch[Cfg]] =
    this.asInstanceOf[PatcherUsing[A, Patch, IgnoreNoneInPatch[Cfg]]]

  /** In case that patch object contains a redundant field (i.e. field that
    * is not present in patched object type), this option enables ignoring
    * value of such fields and generate patch successfully.
    *
    * By default, when Chimney detects a redundant field in patch object, it
    * fails the compilation in order to prevent silent oversight of field name
    * typos.
    *
    * @see [[https://chimney.readthedocs.io/patchers/redundant-fields.html]] for more details
    * @return [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.4.0
    */
  def ignoreRedundantPatcherFields: PatcherUsing[A, Patch, IgnoreRedundantPatcherFields[Cfg]] =
    this.asInstanceOf[PatcherUsing[A, Patch, IgnoreRedundantPatcherFields[Cfg]]]

  /** Enable printing the logs from the derivation process.
    *
    * @see [[https://chimney.readthedocs.io/troubleshooting/debugging-macros.html]] for more details
    *
    * @since 0.8.0
    */
  def enableMacrosLogging: PatcherUsing[A, Patch, MacrosLogging[Cfg]] =
    this.asInstanceOf[PatcherUsing[A, Patch, MacrosLogging[Cfg]]]

  /** Applies configured patching in-place
    *
    * @return patched value
    *
    * @since 0.4.0
    */
  inline def patch: A = ${ PatcherMacros.derivePatcherResult[A, Patch, Cfg]('obj, 'objPatch) }
}
