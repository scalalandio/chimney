package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.collection.compat.*

trait IterableOrArraysPlatform extends IterableOrArrays { this: DefinitionsPlatform =>

  protected object IterableOrArray extends IterableOrArrayModule {

    def buildInIArraySupport[M: Type]: Option[Existential[IterableOrArray[M, *]]] =
      Type.IArray.unapply(Type[M]).map { inner =>
        import inner.Underlying as Inner
        Existential[IterableOrArray[M, *], Inner](
          new IterableOrArray[M, Inner] {
            def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
              m.upcastToExprOf[IArray[Inner]].iterator

            def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr =
              ExistentialExpr.withoutType(m.upcastToExprOf[IArray[Inner]].map(f))

            def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
              m.upcastToExprOf[IArray[Inner]].to(factory)

            override def toString: String = s"support build-in for IArray-type ${Type.prettyPrint[M]}"
          }
        )
      }
  }
}
