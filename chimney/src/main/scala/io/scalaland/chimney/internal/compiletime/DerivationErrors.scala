package io.scalaland.chimney.internal.compiletime

import hearth.fp.data.NonEmptyVector

/** Non-empty collection of derivation errors.
  *
  * Hearth-based port of the pre-Hearth `io.scalaland.chimney.internal.compiletime.DerivationErrors`. The old dedicated
  * `case class DerivationErrors(head, tail)` container is REPLACED by MIO's `MErrors` (`NonEmptyVector[Throwable]`,
  * aliased as `DerivationErrors` in the package object) so that errors flow through MIO natively:
  *   - construction keeps the old shape: `DerivationErrors(error, errors*)`,
  *   - `++` comes from `NonEmptyVector` itself (same aggregation order as the old implementation),
  *   - `prettyPrint`/`asVector` are provided by [[DerivationErrorsOps]] in the package object (non-`DerivationError`
  *     throwables are classified via [[DerivationError.fromThrowable]] at rendering time).
  */
private[compiletime] object DerivationErrors {

  def apply(error: DerivationError, errors: DerivationError*): DerivationErrors =
    NonEmptyVector[Throwable](error, errors.toVector)

  /** Keeps the old `case DerivationErrors(head, tail)` patterns working on `MErrors` (`NonEmptyVector[Throwable]`,
    * which is a `(head, tail)` case class itself) - used e.g. by `TransformProductToProductRule.useOverrideIfPresentOr`
    * to detect a single specific error.
    */
  def unapply(errors: DerivationErrors): Some[(Throwable, Vector[Throwable])] = Some((errors.head, errors.tail))
}
