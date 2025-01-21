package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.internal.runtime.TransformerFlags.*

import scala.annotation.unused

/** Type-level representation of derivation flags which can be enabled/disabled for a specific transformation, a
  * specific source path of a transformation or globally.
  *
  * @since 1.6.0
  */
private[chimney] trait TransformerSourceFlagsDsl[UpdateFlag[_ <: TransformerFlags], Flags <: TransformerFlags] {

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

  /** Enable policy check for target subtypes that would not be used anywhere during transformation.
    *
    * @param unmatchedSubtypePolicy
    *   parameter specifying how unmatched target subtypes should be treated
    *
    * @see
    *   [[TODO]] for more details
    *
    * @since 1.7.0
    */
  def enableUnmatchedSubtypePolicyCheck[P <: UnmatchedSubtypePolicy & Singleton](
      @unused unmatchedSubtypePolicy: P
  ): UpdateFlag[Enable[UnmatchedSubtypePolicyCheck[P], Flags]] =
    enableFlag[UnmatchedSubtypePolicyCheck[P]]

  /** Disable policy check for target subtypes that would not be used anywhere during transformation.
    *
    * @see
    *   [[TODO]] for more details
    *
    * @since 1.7.0
    */
  def disableUnmatchedSubtypePolicyCheck: UpdateFlag[Disable[UnmatchedSubtypePolicyCheck[?], Flags]] =
    disableFlag[UnmatchedSubtypePolicyCheck[?]]

  protected def castedSource: Any = this

  private def enableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Enable[F, Flags]] =
    castedSource.asInstanceOf[UpdateFlag[Enable[F, Flags]]]

  private def disableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Disable[F, Flags]] =
    castedSource.asInstanceOf[UpdateFlag[Disable[F, Flags]]]
}
object TransformerSourceFlagsDsl {

  // It's ugly but:
  // - it works between 2.12/2.13/3
  // - it let us work around limitations of existential types that we have to use in return types fof whitebox macros on Scala 2
  // - it let us work around lack of existential types on Scala 3

  final class OfTransformerInto[
      From,
      To,
      Overrides <: TransformerOverrides,
      Flags <: TransformerFlags,
      FromPath <: Path
  ](
      override protected val castedSource: Any
  ) extends TransformerSourceFlagsDsl[
        ({
          type At[SourceFlags <: TransformerFlags] =
            TransformerInto[From, To, Overrides, Source[FromPath, SourceFlags, Flags]]
        })#At,
        Flags
      ]

  final class OfTransformerDefinition[
      From,
      To,
      Overrides <: TransformerOverrides,
      Flags <: TransformerFlags,
      FromPath <: Path
  ](
      override protected val castedSource: Any
  ) extends TransformerSourceFlagsDsl[
        ({
          type At[SourceFlags <: TransformerFlags] =
            TransformerDefinition[From, To, Overrides, Source[FromPath, SourceFlags, Flags]]
        })#At,
        Flags
      ]

  final class OfPartialTransformerInto[
      From,
      To,
      Overrides <: TransformerOverrides,
      Flags <: TransformerFlags,
      FromPath <: Path
  ](
      override protected val castedSource: Any
  ) extends TransformerSourceFlagsDsl[
        ({
          type At[SourceFlags <: TransformerFlags] =
            PartialTransformerInto[From, To, Overrides, Source[FromPath, SourceFlags, Flags]]
        })#At,
        Flags
      ]

  final class OfPartialTransformerDefinition[
      From,
      To,
      Overrides <: TransformerOverrides,
      Flags <: TransformerFlags,
      FromPath <: Path
  ](
      override protected val castedSource: Any
  ) extends TransformerSourceFlagsDsl[
        ({
          type At[SourceFlags <: TransformerFlags] =
            PartialTransformerDefinition[From, To, Overrides, Source[FromPath, SourceFlags, Flags]]
        })#At,
        Flags
      ]
}
