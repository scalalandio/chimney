package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

import scala.collection.compat.Factory

trait IterableOrArrays { this: Definitions =>

  import Type.Implicits.*

  /** Something allowing us to dispatch same-looking-source-code-but-different ASTs for Iterables and Arrays */
  abstract protected class IterableOrArray[M, A] {
    def iterator(m: Expr[M]): Expr[Iterator[A]]

    def map[B: Type](m: Expr[M])(f: Expr[A => B]): ExistentialExpr

    def to[C: Type](m: Expr[M])(factory: Expr[Factory[A, C]]): Expr[C]
  }
  protected object IterableOrArray {

    def unapply[M](implicit tpe: Type[M]): Option[Existential[IterableOrArray[M, *]]] = tpe match {
      case Type.Map(k, v) =>
        import k.Underlying as K, v.Underlying as V
        val a = ExistentialType[(k.Underlying, v.Underlying)]
        import a.Underlying as Inner
        Some(
          Existential[IterableOrArray[M, *], a.Underlying](
            new IterableOrArray[M, a.Underlying] {

              def iterator(m: Expr[M]): Expr[Iterator[a.Underlying]] =
                m.widenExpr[Iterable[a.Underlying]].iterator

              def map[B: Type](m: Expr[M])(f: Expr[a.Underlying => B]): ExistentialExpr =
                ExistentialExpr.withoutType(m.widenExpr[Iterable[a.Underlying]].map(f))

              def to[C: Type](m: Expr[M])(factory: Expr[Factory[a.Underlying, C]]): Expr[C] =
                m.widenExpr[Iterable[a.Underlying]].to(factory)
            }
          )
        )
      case Type.Iterable(a) =>
        import a.Underlying as Inner
        Some(
          Existential[IterableOrArray[M, *], a.Underlying](
            new IterableOrArray[M, a.Underlying] {

              def iterator(m: Expr[M]): Expr[Iterator[a.Underlying]] =
                m.widenExpr[Iterable[a.Underlying]].iterator

              def map[B: Type](m: Expr[M])(f: Expr[a.Underlying => B]): ExistentialExpr =
                ExistentialExpr.withoutType(m.widenExpr[Iterable[a.Underlying]].map(f))

              def to[C: Type](m: Expr[M])(factory: Expr[Factory[a.Underlying, C]]): Expr[C] =
                m.widenExpr[Iterable[a.Underlying]].to(factory)
            }
          )
        )
      case Type.Array(a) =>
        import a.Underlying as Inner
        Some(
          Existential[IterableOrArray[M, *], a.Underlying](
            new IterableOrArray[M, a.Underlying] {
              def iterator(m: Expr[M]): Expr[Iterator[a.Underlying]] =
                m.widenExpr[Array[a.Underlying]].iterator

              def map[B: Type](m: Expr[M])(f: Expr[a.Underlying => B]): ExistentialExpr =
                ExistentialExpr.withoutType(m.widenExpr[Array[a.Underlying]].map(f))

              def to[C: Type](m: Expr[M])(factory: Expr[Factory[a.Underlying, C]]): Expr[C] =
                m.widenExpr[Array[a.Underlying]].to(factory)
            }
          )
        )
      case _ => None
    }
  }
}
