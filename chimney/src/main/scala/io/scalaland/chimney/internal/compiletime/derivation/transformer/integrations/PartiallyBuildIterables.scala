package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

trait PartiallyBuildIterables { this: Derivation =>

  import ChimneyType.Implicits.*

  /** Something allowing us to share the logic which handles NonEmptyList, NonEmptySet, ... and whatever we want to
    * support.
    *
    * Tries to use [[io.scalaland.chimney.integrations.PartiallyBuildIterable]], if type is eligible.
    */
  abstract protected class PartiallyBuildIterable[Collection, Item]
      extends TotallyOrPartiallyBuildIterable[Collection, Item] {

    def factory: Either[Expr[Factory[Item, Collection]], Expr[Factory[Item, partial.Result[Collection]]]] = Right(
      partialFactory
    )

    def partialFactory: Expr[Factory[Item, partial.Result[Collection]]]

    def iterator(collection: Expr[Collection]): Expr[Iterator[Item]]

    def to[Collection2: Type](
        collection: Expr[Collection],
        factory: Expr[Factory[Item, Collection2]]
    ): Expr[Collection2]

    val asMap: Option[(ExistentialType, ExistentialType)]
  }
  protected object PartiallyBuildIterable {

    def unapply[M](implicit M: Type[M]): Option[Existential[PartiallyBuildIterable[M, *]]] =
      providedSupport[M]

    private def providedSupport[Collection: Type]: Option[Existential[PartiallyBuildIterable[Collection, *]]] =
      summonPartiallyBuildIterable[Collection].map { partiallyBuildIterable =>
        import partiallyBuildIterable.{Underlying as Item, value as partiallyBuildIterableExpr}
        Existential[PartiallyBuildIterable[Collection, *], Item](
          new PartiallyBuildIterable[Collection, Item] {

            def partialFactory: Expr[Factory[Item, partial.Result[Collection]]] =
              partiallyBuildIterableExpr.partialFactory

            def iterator(collection: Expr[Collection]): Expr[Iterator[Item]] =
              partiallyBuildIterableExpr.iterator(collection)

            def to[Collection2: Type](
                collection: Expr[Collection],
                factory: Expr[Factory[Item, Collection2]]
            ): Expr[Collection2] = partiallyBuildIterableExpr.to(collection, factory)

            val asMap: Option[(ExistentialType, ExistentialType)] = partiallyBuildIterableExpr.tpe match {
              case ChimneyType.PartiallyBuildMap(_, key, value) => Some(key -> value)
              case _                                            => None
            }

            override def toString: String = s"support provided by ${Expr.prettyPrint(partiallyBuildIterableExpr)}"
          }
        )
      }
  }
}
