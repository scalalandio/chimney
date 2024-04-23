package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

trait TotallyBuildIterables { this: Derivation =>

  import ChimneyType.Implicits.*

  /** Something allowing us to share the logic which handles [[scala.collection.Iterable]], [[scala.Array]],
    * [[java.util.Collection]], ... and whatever we want to support.
    *
    * Tries to use [[io.scalaland.chimney.integrations.TotallyBuildIterable]] and then falls back on [[IterableOrArray]]
    * hardcoded support, if type is eligible.
    */
  abstract protected class TotallyBuildIterable[Collection, Item]
      extends TotallyOrPartiallyBuildIterable[Collection, Item] {

    def factory: Either[Expr[Factory[Item, Collection]], Expr[Factory[Item, partial.Result[Collection]]]] = Left(
      totalFactory
    )

    def totalFactory: Expr[Factory[Item, Collection]]

    def iterator(collection: Expr[Collection]): Expr[Iterator[Item]]

    def to[Collection2: Type](
        collection: Expr[Collection],
        factory: Expr[Factory[Item, Collection2]]
    ): Expr[Collection2]

    val asMap: Option[(ExistentialType, ExistentialType)]
  }
  protected object TotallyBuildIterable {

    def unapply[M](implicit M: Type[M]): Option[Existential[TotallyBuildIterable[M, *]]] =
      providedSupport[M].orElse(buildInSupport[M])

    private def providedSupport[Collection: Type]: Option[Existential[TotallyBuildIterable[Collection, *]]] =
      summonTotallyBuildIterable[Collection].map { totallyBuildIterable =>
        import totallyBuildIterable.{Underlying as Item, value as totallyBuildIterableExpr}
        Existential[TotallyBuildIterable[Collection, *], Item](
          new TotallyBuildIterable[Collection, Item] {

            def totalFactory: Expr[Factory[Item, Collection]] =
              totallyBuildIterableExpr.totalFactory

            def iterator(collection: Expr[Collection]): Expr[Iterator[Item]] =
              totallyBuildIterableExpr.iterator(collection)

            def to[Collection2: Type](
                collection: Expr[Collection],
                factory: Expr[Factory[Item, Collection2]]
            ): Expr[Collection2] = totallyBuildIterableExpr.to(collection, factory)

            val asMap: Option[(ExistentialType, ExistentialType)] = totallyBuildIterableExpr.tpe match {
              case ChimneyType.TotallyBuildMap(_, key, value) => Some(key -> value)
              case _                                          => None
            }

            override def toString: String = s"support provided by ${Expr.prettyPrint(totallyBuildIterableExpr)}"
          }
        )
      }

    private def buildInSupport[M: Type]: Option[Existential[TotallyBuildIterable[M, *]]] =
      IterableOrArray.unapply[M].map { found =>
        import found.{Underlying as Item, value as iora}
        Existential[TotallyBuildIterable[M, *], Item](
          new TotallyBuildIterable[M, Item] {

            def totalFactory: Expr[Factory[Item, M]] =
              iora.factory

            def iterator(collection: Expr[M]): Expr[Iterator[Item]] =
              iora.iterator(collection)

            def to[Collection2: Type](
                collection: Expr[M],
                factory: Expr[Factory[Item, Collection2]]
            ): Expr[Collection2] = iora.to(collection)(factory)

            val asMap: Option[(ExistentialType, ExistentialType)] = Type[M] match {
              case Type.Map(key, value) => Some(key -> value)
              case _                    => None
            }

            override def toString: String = iora.toString
          }
        )
      }
  }
}
