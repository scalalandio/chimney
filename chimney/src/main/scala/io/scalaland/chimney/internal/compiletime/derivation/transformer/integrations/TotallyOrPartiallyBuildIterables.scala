package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

trait TotallyOrPartiallyBuildIterables { this: Derivation =>

  /** Parent of [[TotallyBuildIterable]] and [[PartiallyBuildIterable]] which gives access to reading values from Expr.
    */
  abstract protected class TotallyOrPartiallyBuildIterable[Collection, Item] {

    def factory: Either[Expr[Factory[Item, Collection]], Expr[Factory[Item, partial.Result[Collection]]]]

    def iterator(collection: Expr[Collection]): Expr[Iterator[Item]]

    def to[Collection2: Type](
        collection: Expr[Collection],
        factory: Expr[Factory[Item, Collection2]]
    ): Expr[Collection2]

    val asMap: Option[(ExistentialType, ExistentialType)]
  }
  object TotallyOrPartiallyBuildIterable {

    def unapply[M](implicit M: Type[M]): Option[Existential[TotallyOrPartiallyBuildIterable[M, *]]] =
      TotallyBuildIterable
        .unapply[M]
        .asInstanceOf[Option[Existential[TotallyOrPartiallyBuildIterable[M, *]]]]
        .orElse(
          PartiallyBuildIterable.unapply[M].asInstanceOf[Option[Existential[TotallyOrPartiallyBuildIterable[M, *]]]]
        )
  }
}
