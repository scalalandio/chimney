package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.internal.NotSupportedTransformerDerivation

private[compiletime] trait Results { this: Definitions =>

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

  protected def reportInfo(info: String): Unit

  protected def reportError(errors: String): Nothing
}
