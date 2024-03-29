package io.scalaland.chimney.internal.compiletime

// TODO: create fp.NonEmptyVector because it might be useful

/** Non-empty list of errors */
final private[compiletime] case class DerivationErrors(head: DerivationError, tail: Vector[DerivationError]) {

  def ++(errors: DerivationErrors): DerivationErrors =
    DerivationErrors(head, tail ++ Vector(errors.head) ++ errors.tail)

  def prettyPrint: String = DerivationError.printErrors(head +: tail)

  def asVector: Vector[DerivationError] = head +: tail
}
private[compiletime] object DerivationErrors {

  def apply(error: DerivationError, errors: DerivationError*): DerivationErrors =
    apply(error, errors.toVector)
}
