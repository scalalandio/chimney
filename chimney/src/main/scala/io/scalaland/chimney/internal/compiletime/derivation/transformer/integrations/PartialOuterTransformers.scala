package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.integrations
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

trait PartialOuterTransformers { this: Derivation =>

  abstract protected class PartialOuterTransformer[From: Type, To: Type] {
    type InnerFrom
    implicit val InnerFrom: Type[InnerFrom]

    type InnerTo
    implicit val InnerTo: Type[InnerTo]

    val instance: Expr[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]]

    def transformWithTotalInner(
        src: Expr[From],
        failFast: Expr[Boolean],
        inner: Expr[InnerFrom => InnerTo]
    ): Expr[partial.Result[To]] =
      instance.transformWithTotalInner(src, failFast, inner)

    def transformWithPartialInner(
        src: Expr[From],
        failFast: Expr[Boolean],
        inner: Expr[InnerFrom => partial.Result[InnerTo]]
    ): Expr[partial.Result[To]] = instance.transformWithPartialInner(src, failFast, inner)
  }
  protected object PartialOuterTransformer {

    def unapply[From, To](implicit from: Type[From], to: Type[To]): Option[PartialOuterTransformer[From, To]] =
      summonPartialOuterTransformer[From, To]
  }
}
