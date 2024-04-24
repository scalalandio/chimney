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
    def factory: Expr[Factory[A, M]]

    def iterator(m: Expr[M]): Expr[Iterator[A]]

    def map[B: Type](m: Expr[M])(f: Expr[A => B]): ExistentialExpr

    def to[C: Type](m: Expr[M])(factory: Expr[Factory[A, C]]): Expr[C]
  }
  protected val IterableOrArray: IterableOrArrayModule
  protected trait IterableOrArrayModule { this: IterableOrArray.type =>

    def unapply[M](implicit M: Type[M]): Option[Existential[IterableOrArray[M, *]]] = M match {
      case Type.Map(k, v) =>
        import k.Underlying as K, v.Underlying as V
        val a = ExistentialType[(K, V)]
        import a.Underlying as Inner
        Some(buildInIterableSupport[M, Inner](s"support build-in for Map-type ${Type.prettyPrint[M]}"))
      case Type.Iterable(a) =>
        import a.Underlying as Inner
        Some(buildInIterableSupport[M, Inner](s"support build-in for Iterable-type ${Type.prettyPrint[M]}"))
      case Type.Iterator(a) =>
        import a.Underlying as Inner
        Some(buildInIteratorSupport[M, Inner])
      case Type.Array(a) =>
        import a.Underlying as Inner
        Some(buildInArraySupport[M, Inner])
      case _ => buildInIArraySupport[M]
    }

    private def buildInIterableSupport[M: Type, Inner: Type](hint: String): Existential[IterableOrArray[M, *]] =
      Existential[IterableOrArray[M, *], Inner](
        new IterableOrArray[M, Inner] {
          def factory: Expr[Factory[Inner, M]] = Expr.summonImplicitUnsafe[Factory[Inner, M]]

          def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
            m.upcastToExprOf[Iterable[Inner]].iterator

          def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr =
            ExistentialExpr.withoutType(m.upcastToExprOf[Iterable[Inner]].map(f))

          def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
            m.upcastToExprOf[Iterable[Inner]].to(factory)

          override def toString: String = hint
        }
      )

    private def buildInArraySupport[M: Type, Inner: Type]: Existential[IterableOrArray[M, *]] =
      Existential[IterableOrArray[M, *], Inner](
        new IterableOrArray[M, Inner] {
          def factory: Expr[Factory[Inner, M]] = Expr.summonImplicitUnsafe[Factory[Inner, M]]

          def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
            m.upcastToExprOf[Array[Inner]].iterator

          def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr =
            ExistentialExpr.withoutType(m.upcastToExprOf[Array[Inner]].map(f))

          def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
            m.upcastToExprOf[Array[Inner]].to(factory)

          override def toString: String = s"support build-in for Array-type ${Type.prettyPrint[M]}"
        }
      )

    private def buildInIteratorSupport[M: Type, Inner: Type]: Existential[IterableOrArray[M, *]] =
      Existential[IterableOrArray[M, *], Inner](
        new IterableOrArray[M, Inner] {
          def factory: Expr[Factory[Inner, M]] = Expr.summonImplicitUnsafe[Factory[Inner, M]]

          def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
            m.upcastToExprOf[Iterator[Inner]]

          def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr =
            ExistentialExpr.withoutType(m.upcastToExprOf[Iterator[Inner]].map(f))

          def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
            m.upcastToExprOf[Iterator[Inner]].to(factory)

          override def toString: String = s"support build-in for Iterator-type ${Type.prettyPrint[M]}"
        }
      )

    protected def buildInIArraySupport[M: Type]: Option[Existential[IterableOrArray[M, *]]]
  }
}
