package io.scalaland.chimney.internal.compiletime

import hearth.fp.data.NonEmptyVector

/** Non-empty collection of derivation errors - MIO's `MErrors` (`NonEmptyVector[Throwable]`, aliased in the package
  * object) so that errors flow through MIO natively; `prettyPrint`/`asVector` come from [[ResultSyntax]].
  */
private[compiletime] object DerivationErrors {

  def apply(error: DerivationError, errors: DerivationError*): DerivationErrors =
    NonEmptyVector[Throwable](error, errors.toVector)

  /** Allows `case DerivationErrors(head, tail)` patterns on `MErrors` - used e.g. by
    * `TransformProductToProductRule.useOverrideIfPresentOr` to detect a single specific error.
    */
  def unapply(errors: DerivationErrors): Some[(Throwable, Vector[Throwable])] = Some((errors.head, errors.tail))
}
