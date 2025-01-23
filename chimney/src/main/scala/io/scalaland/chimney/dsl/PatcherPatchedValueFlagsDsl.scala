package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, Path}
import io.scalaland.chimney.internal.runtime.PatcherFlags.*

/** Type-level representation of derivation flags which can be enabled/disabled for a specific transformation, a
  * specific target path of a transformation or globally.
  *
  * @since 1.7.0
  */
private[chimney] trait PatcherPatchedValueFlagsDsl[UpdateFlag[_ <: PatcherFlags], Flags <: PatcherFlags] {

  /** In case when both object to patch and patch value contain field of type [[scala.Option]], this option allows to
    * treat [[scala.None]] value in patch as if the value was not provided.
    *
    * By default, when [[scala.None]] is delivered in patch, Chimney clears the value of such field on patching.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#treating-none-as-no-update-instead-of-set-to-none]] for
    *   more details
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
    *   [[https://chimney.readthedocs.io/supported-patching/#treating-left-as-no-update-instead-of-set-to-left]] for
    *   more details
    *
    * @since 1.7.0
    */
  def ignoreLeftInPatch: UpdateFlag[Enable[IgnoreLeftInPatch, Flags]] =
    enableFlag[IgnoreLeftInPatch]

  /** When [[scala.Either]] is patching [[scala.Either]], on [[scala.Left]] value will be overrides.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#treating-left-as-no-update-instead-of-set-to-left]] for
    *   more details
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
    *   [[https://chimney.readthedocs.io/supported-patching/#appending-to-collection-instead-of-replacing-it]] for more
    *   details
    *
    * @since 1.7.0
    */
  def appendCollectionInPatch: UpdateFlag[Enable[AppendCollectionInPatch, Flags]] =
    enableFlag[AppendCollectionInPatch]

  /** When collection is patching collection, the value will be simply overriden.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#appending-to-collection-instead-of-replacing-it]] for more
    *   details
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
    * @since 0.4.0
    */
  def ignoreRedundantPatcherFields: UpdateFlag[Enable[IgnoreRedundantPatcherFields, Flags]] =
    enableFlag[IgnoreRedundantPatcherFields]

  /** Fail the compilation if there is a redundant field in patching object.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#ignoring-fields-in-patches]] for more details
    *
    * @since 0.8.0
    */
  def failRedundantPatcherFields: UpdateFlag[Disable[IgnoreRedundantPatcherFields, Flags]] =
    disableFlag[IgnoreRedundantPatcherFields]

  protected def castedTarget: Any = this

  private def enableFlag[F <: PatcherFlags.Flag]: UpdateFlag[Enable[F, Flags]] =
    castedTarget.asInstanceOf[UpdateFlag[Enable[F, Flags]]]

  private def disableFlag[F <: PatcherFlags.Flag]: UpdateFlag[Disable[F, Flags]] =
    castedTarget.asInstanceOf[UpdateFlag[Disable[F, Flags]]]
}
object PatcherPatchedValueFlagsDsl {

  // It's ugly but:
  // - it works between 2.12/2.13/3
  // - it let us work around limitations of existential types that we have to use in return types fof whitebox macros on Scala 2
  // - it let us work around lack of existential types on Scala 3

  final class OfPatcherUsing[A, Patch, Overrides <: PatcherOverrides, Flags <: PatcherFlags, ObjPath <: Path](
      override protected val castedTarget: Any
  ) extends PatcherPatchedValueFlagsDsl[
        ({
          type At[PatchedValueFlags <: PatcherFlags] =
            PatcherUsing[A, Patch, Overrides, PatchedValue[ObjPath, PatchedValueFlags, Flags]]
        })#At,
        Flags
      ]

  final class OfPatcherDefinition[
      A,
      Patch,
      Overrides <: PatcherOverrides,
      Flags <: PatcherFlags,
      ObjPath <: Path
  ](
      override protected val castedTarget: Any
  ) extends PatcherPatchedValueFlagsDsl[
        ({
          type At[PatchedValueFlags <: PatcherFlags] =
            PatcherDefinition[A, Patch, Overrides, PatchedValue[ObjPath, PatchedValueFlags, Flags]]
        })#At,
        Flags
      ]
}
