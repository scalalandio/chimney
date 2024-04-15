package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

import scala.collection.compat.Factory

trait IterableOrArrays { this: Definitions =>

  import Type.Implicits.*

  /** Something allowing us to dispatch same-looking-source-code-but-different ASTs for Iterables and Arrays.
    *
    * Exists because `Array` is NOT `Iterable`, and all operations like `.map`, `.to`, etc are done through extension
    * methods. Meanwhile, we would like to be able to convert to and from Array easily.
    */
  abstract protected class IterableOrArray[M, A] {
    def iterator(m: Expr[M]): Expr[Iterator[A]]

    def map[B: Type](m: Expr[M])(f: Expr[A => B]): ExistentialExpr

    def to[C: Type](m: Expr[M])(factory: Expr[Factory[A, C]]): Expr[C]
  }
  protected object IterableOrArray {

    def unapply[M](implicit tpe: Type[M]): Option[Existential[IterableOrArray[M, *]]] = tpe match {
      case Type.Map(k, v) =>
        import k.Underlying as K, v.Underlying as V
        val a = ExistentialType[(K, V)]
        import a.Underlying as Inner
        Some(
          Existential[IterableOrArray[M, *], Inner](
            new IterableOrArray[M, Inner] {

              def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
                m.upcastToExprOf[Iterable[Inner]].iterator

              def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr =
                ExistentialExpr.withoutType(m.upcastToExprOf[Iterable[Inner]].map(f))

              def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
                m.upcastToExprOf[Iterable[Inner]].to(factory)

              override def toString: String = s"support build-in for map-type ${Type.prettyPrint[M]}"
            }
          )
        )
      case Type.Iterable(a) =>
        import a.Underlying as Inner
        Some(
          Existential[IterableOrArray[M, *], Inner](
            new IterableOrArray[M, Inner] {

              def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
                m.upcastToExprOf[Iterable[Inner]].iterator

              def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr =
                ExistentialExpr.withoutType(m.upcastToExprOf[Iterable[Inner]].map(f))

              def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
                m.upcastToExprOf[Iterable[Inner]].to(factory)

              override def toString: String = s"support build-in for iterable-type ${Type.prettyPrint[M]}"
            }
          )
        )
      case Type.Array(a) =>
        import a.Underlying as Inner
        Some(
          Existential[IterableOrArray[M, *], Inner](
            new IterableOrArray[M, Inner] {
              def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
                m.upcastToExprOf[Array[Inner]].iterator

              def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr =
                ExistentialExpr.withoutType(m.upcastToExprOf[Array[Inner]].map(f))

              def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
                m.upcastToExprOf[Array[Inner]].to(factory)

              override def toString: String = s"support build-in for array-type ${Type.prettyPrint[M]}"
            }
          )
        )
      case _ => None
    }
  }
}
