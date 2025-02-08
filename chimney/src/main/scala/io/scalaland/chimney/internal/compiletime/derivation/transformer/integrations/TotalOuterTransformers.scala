package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.integrations
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

trait TotalOuterTransformers { this: Derivation =>

  import ChimneyType.Implicits.*

  abstract protected class TotalOuterTransformer[From: Type, To: Type] {
    type InnerFrom
    implicit val InnerFrom: Type[InnerFrom]

    type InnerTo
    implicit val InnerTo: Type[InnerTo]

    val instance: Expr[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]]

    def transformWithTotalInner(
        src: Expr[From],
        inner: Expr[InnerFrom => InnerTo]
    ): Expr[To] = instance.transformWithTotalInner(src, inner)

    def transformWithPartialInner(
        src: Expr[From],
        failFast: Expr[Boolean],
        inner: Expr[InnerFrom => partial.Result[InnerTo]]
    ): Expr[partial.Result[To]] = instance.transformWithPartialInner(src, failFast, inner)
  }
  protected object TotalOuterTransformer {

    private val implicitCache = new Type.Cache[Option]
    def unapply[From, To](implicit from: Type[From], to: Type[To]): Option[TotalOuterTransformer[From, To]] =
      implicitCache(
        Type[integrations.TotalOuterTransformer[From, To, From, To]].asInstanceOf[Type[TotalOuterTransformer[From, To]]]
      )(summonTotalOuterTransformer[From, To])
  }
}
