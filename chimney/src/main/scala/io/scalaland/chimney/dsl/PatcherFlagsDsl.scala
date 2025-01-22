package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.PatcherFlags.*
import io.scalaland.chimney.internal.runtime.PatcherFlags

/** Type-level representation of derivation flags which can be enabled/disabled for a specific patching or globally.
  *
  * @since 0.8.0
  */
private[dsl] trait PatcherFlagsDsl[UpdateFlag[_ <: PatcherFlags], Flags <: PatcherFlags]
    extends PatcherPatchedValueFlagsDsl[UpdateFlag, Flags] {

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
