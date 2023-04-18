package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Results { this: Definitions =>

  implicit class DerivationResultMethods[A](derivationResult: DerivationResult[A]) {

    final def unsafeGet: (DerivationResult.State, A) = derivationResult.toEither match {
      case (state, Right(value))       => state -> value
      case (_, Left(derivationErrors)) => reportError(derivationErrors.prettyPrint)
    }
  }

  protected def reportError(errors: String): Nothing
}
