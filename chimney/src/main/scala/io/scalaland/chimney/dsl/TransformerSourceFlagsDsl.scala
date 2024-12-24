package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.TransformerFlags
import io.scalaland.chimney.internal.runtime.TransformerFlags.*

import scala.annotation.unused

/** Type-level representation of derivation flags which can be enabled/disabled for a specific transformation, a
  * specific source path of a transformation or globally.
  *
  * @since 1.6.0
  */
private[dsl] trait TransformerSourceFlagsDsl[UpdateFlag[_ <: TransformerFlags], Flags <: TransformerFlags] {

  /** Enable custom way of comparing if source subtypes' names and target fields' names are matching.
    *
    * @param namesComparison
    *   parameter specifying how names should be compared by macro
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#customizing-subtype-name-matching]] for more details
    *
    * @since 1.0.0
    */
  def enableCustomSubtypeNameComparison[C <: TransformedNamesComparison & Singleton](
      @unused namesComparison: C
  ): UpdateFlag[Enable[SubtypeNameComparison[C], Flags]] =
    enableFlag[SubtypeNameComparison[C]]

  /** Disable any custom way of comparing if source subtypes' names and target fields' names are matching.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#customizing-subtype-name-matching]] for more details
    *
    * @since 1.0.0
    */
  def disableCustomSubtypeNameComparison: UpdateFlag[Disable[SubtypeNameComparison[?], Flags]] =
    disableFlag[SubtypeNameComparison[?]]

  private def enableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Enable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Enable[F, Flags]]]

  private def disableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Disable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Disable[F, Flags]]]
}
