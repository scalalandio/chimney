package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.internal.NotSupportedTransformerDerivation

private[compiletime] trait Results { this: Definitions =>

  implicit class DerivationResultMethods[A](derivationResult: DerivationResult[A]) {

//    final def unsafeGet: (DerivationResult.State, A) = derivationResult.toEither match {
//      case (state, Right(value))       => state -> value
//      case (_, Left(derivationErrors)) => reportError(derivationErrors.prettyPrint)
//    }
  }
  implicit class DerivationResultModule(derivationResult: DerivationResult.type) {

    def notSupportedTransformerDerivation[From, To, T](implicit
        ctx: TransformerContext[From, To]
    ): DerivationResult[T] =
      DerivationResult.transformerError(
        NotSupportedTransformerDerivation(
          fieldName = Expr.prettyPrint(ctx.src),
          sourceTypeName = Type.prettyPrint[From],
          targetTypeName = Type.prettyPrint[To]
        )
      )
  }

  protected def reportError(errors: String): Nothing
}
