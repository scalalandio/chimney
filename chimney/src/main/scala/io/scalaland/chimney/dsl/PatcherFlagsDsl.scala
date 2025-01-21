package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.PatcherFlags.*
import io.scalaland.chimney.internal.runtime.PatcherFlags

/** Type-level representation of derivation flags which can be enabled/disabled for a specific patching or globally.
  *
  * @since 0.8.0
  */
private[dsl] trait PatcherFlagsDsl[UpdateFlag[_ <: PatcherFlags], Flags <: PatcherFlags] {

  /** In case when both object to patch and patch value contain field of type [[scala.Option]], this option allows to
    * treat [[scala.None]] value in patch as if the value was not provided.
    *
    * By default, when [[scala.None]] is delivered in patch, Chimney clears the value of such field on patching.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#treating-none-as-no-update-instead-of-set-to-none]] for
    *   more details
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.4.0
    */
  def ignoreNoneInPatch: UpdateFlag[Enable[IgnoreNoneInPatch, Flags]] =
    enableFlag[IgnoreNoneInPatch]

  /** When [[scala.Option]] is patching [[scala.Option]], on [[scala.None]] value will be cleared.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#treating-none-as-no-update-instead-of-set-to-none]] for
    *   more details
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.8.0
    */
  def clearOnNoneInPatch: UpdateFlag[Disable[IgnoreNoneInPatch, Flags]] =
    disableFlag[IgnoreNoneInPatch]

  /** In case when both object to patch and patch value contain field of type [[scala.Either]], this option allows to
    * treat [[scala.Left]] value in patch as if the value was not provided.
    *
    * By default, when [[scala.Left]] is delivered in patch, Chimney used this new value.
    *
    * @see
    *   TODO
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 1.7.0
    */
  def ignoreLeftInPatch: UpdateFlag[Enable[IgnoreLeftInPatch, Flags]] =
    enableFlag[IgnoreLeftInPatch]

  /** When [[scala.Either]] is patching [[scala.Either]], on [[scala.Left]] value will be overrides.
    *
    * @see
    *   TODO
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 1.7.0
    */
  def useLeftOnLeftInPatch: UpdateFlag[Disable[IgnoreLeftInPatch, Flags]] =
    disableFlag[IgnoreLeftInPatch]

  /** In case when both object to patch and patch value contain field with a collection, this option allows to append
    * value from patch to the source value, rather than overriding it.
    *
    * By default, patch's collection overrides the content of a field.
    *
    * @see
    *   TODO
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 1.7.0
    */
  def appendCollectionInPatch: UpdateFlag[Enable[AppendCollectionInPatch, Flags]] =
    enableFlag[AppendCollectionInPatch]

  /** When collection is patching collection, the value will be simply overriden.
    *
    * @see
    *   TODO
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 1.7.0
    */
  def overrideCollectionInPatch: UpdateFlag[Disable[AppendCollectionInPatch, Flags]] =
    disableFlag[AppendCollectionInPatch]

  /** In case that patch object contains a redundant field (i.e. field that is not present in patched object type), this
    * option enables ignoring value of such fields and generate patch successfully.
    *
    * By default, when Chimney detects a redundant field in patch object, it fails the compilation in order to prevent
    * silent oversight of field name typos.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#ignoring-fields-in-patches]] for more details
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.4.0
    */
  def ignoreRedundantPatcherFields: UpdateFlag[Enable[IgnoreRedundantPatcherFields, Flags]] =
    enableFlag[IgnoreRedundantPatcherFields]

  /** Fail the compilation if there is a redundant field in patching object.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#ignoring-fields-in-patches]] for more details
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.8.0
    */
  def failRedundantPatcherFields: UpdateFlag[Disable[IgnoreRedundantPatcherFields, Flags]] =
    disableFlag[IgnoreRedundantPatcherFields]

  /** Enable printing the logs from the derivation process.
    *
    * @see
    *   [[https://chimney.readthedocs.io/troubleshooting/#debugging-macros]] for more details
    *
    * @since 0.8.0
    */
  def enableMacrosLogging: UpdateFlag[Enable[MacrosLogging, Flags]] =
    enableFlag[MacrosLogging]

  /** Disable printing the logs from the derivation process.
    *
    * @see
    *   [[https://chimney.readthedocs.io/troubleshooting/#debugging-macros]] for more details
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
