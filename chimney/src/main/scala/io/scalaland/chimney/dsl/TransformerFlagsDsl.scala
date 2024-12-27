package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.TransformerFlags.*
import io.scalaland.chimney.internal.runtime.TransformerFlags

/** Type-level representation of derivation flags which can be enabled/disabled for a specific transformation or
  * globally.
  *
  * @since 0.6.0
  */
private[dsl] trait TransformerFlagsDsl[UpdateFlag[_ <: TransformerFlags], Flags <: TransformerFlags]
    extends TransformerSourceFlagsDsl[UpdateFlag, Flags]
    with TransformerTargetFlagsDsl[UpdateFlag, Flags] {

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

  private def enableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Enable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Enable[F, Flags]]]

  private def disableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Disable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Disable[F, Flags]]]
}
