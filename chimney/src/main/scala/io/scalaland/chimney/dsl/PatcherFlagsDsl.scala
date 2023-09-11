package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.PatcherFlags.*
import io.scalaland.chimney.internal.runtime.PatcherFlags

/** Type-level representation of derivation flags which can be enabled/disabled for a specific patching or globally.
  *
  * @since 0.8.0
  */
private[dsl] trait PatcherFlagsDsl[UpdateFlag[_ <: PatcherFlags], Flags <: PatcherFlags] {

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
  def ignoreNoneInPatch: UpdateFlag[Enable[IgnoreNoneInPatch, Flags]] =
    enableFlag[IgnoreNoneInPatch]

  /** Then there Option is patching Option, on None value will be cleared.
    *
    * @see [[https://chimney.readthedocs.io/patchers/options-handling.html]] for more details
    * @return [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.8.0
    */
  def clearOnNoneInPatch: UpdateFlag[Disable[IgnoreNoneInPatch, Flags]] =
    disableFlag[IgnoreNoneInPatch]

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
  def ignoreRedundantPatcherFields: UpdateFlag[Enable[IgnoreRedundantPatcherFields, Flags]] =
    enableFlag[IgnoreRedundantPatcherFields]

  /** Fail the compilation if there is a redundant field in patching objec.
    *
    * @see [[https://chimney.readthedocs.io/patchers/redundant-fields.html]] for more details
    * @return [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.8.0
    */
  def failRedundantPatcherFields: UpdateFlag[Disable[IgnoreRedundantPatcherFields, Flags]] =
    disableFlag[IgnoreRedundantPatcherFields]

  /** Enable printing the logs from the derivation process.
    *
    * @see [[https://chimney.readthedocs.io/troubleshooting/debugging-macros.html]] for more details
    *
    * @since 0.8.0
    */
  def enableMacrosLogging: UpdateFlag[Enable[MacrosLogging, Flags]] =
    enableFlag[MacrosLogging]

  /** Disable printing the logs from the derivation process.
    *
    * @see [[https://chimney.readthedocs.io/troubleshooting/debugging-macros.html]] for more details
    *
    * @since 0.8.0
    */
  def disableMacrosLogging: UpdateFlag[Disable[MacrosLogging, Flags]] =
    disableFlag[MacrosLogging]

  private def enableFlag[F <: PatcherFlags.Flag]: UpdateFlag[Enable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Enable[F, Flags]]]

  private def disableFlag[F <: PatcherFlags.Flag]: UpdateFlag[Disable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Disable[F, Flags]]]
}
