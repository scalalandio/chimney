package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Exprs { this: Definitions =>

  /** Platform-specific expression representation (c.universe.Expr[A] in 2, quotes.Expr[A] in 3 */
  protected type Expr[A]
  protected object Expr {

    val Unit: Expr[Unit] = exprImpl.Unit

    object Array {
      def apply[A: Type](args: Expr[A]*): Expr[Array[A]] = exprImpl.Array[A](args*)
    }

    object Option {
      def apply[A: Type](a: Expr[A]): Expr[Option[A]] = exprImpl.Option[A](a)
      def empty[A: Type]: Expr[Option[A]] = exprImpl.OptionEmpty[A]
      def apply[A: Type]: Expr[A => Option[A]] = exprImpl.OptionApply[A]
    }
    val None: Expr[scala.None.type] = exprImpl.None

    object Either {
      def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] = exprImpl.Left[L, R](value)
      def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] = exprImpl.Right[L, R](value)
    }

    object PartialResult {
      object Value {
        def apply[T: Type](value: Expr[T]): Expr[partial.Result.Value[T]] = exprImpl.PartialResultValue[T](value)
      }
      object Errors {
        def merge(
            errors1: Expr[partial.Result.Errors],
            errors2: Expr[partial.Result.Errors]
        ): Expr[partial.Result.Errors] = exprImpl.PartialResultErrorsMerge(errors1, errors2)
        def mergeResultNullable[T: Type](
            errorsNullable: Expr[partial.Result.Errors],
            result: Expr[partial.Result[T]]
        ): Expr[partial.Result.Errors] = exprImpl.PartialResultErrorsMergeResultNullable(errorsNullable, result)
      }

      def fromEmpty[T: Type]: Expr[partial.Result[T]] = exprImpl.PartialResultEmpty
      def fromFunction[S: Type, T: Type](f: Expr[S => T]): Expr[S => partial.Result[T]] =
        exprImpl.PartialResultFunction[S, T](f)

      def traverse[M: Type, A: Type, B: Type](
          it: Expr[Iterator[A]],
          f: Expr[A => partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[M]] = exprImpl.PartialResultTraverse[M, A, B](it, f, failFast)
      def sequence[M: Type, A: Type](
          it: Expr[Iterator[partial.Result[A]]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[M]] = exprImpl.PartialResultSequence[M, A](it, failFast)
      def map2[A: Type, B: Type, C: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          f: Expr[(A, B) => C],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[C]] = exprImpl.PartialResultMap2[A, B, C](fa, fb, f, failFast)
      def product[A: Type, B: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[(A, B)]] = exprImpl.PartialResultProduct[A, B](fa, fb, failFast)
    }

    object PathElement {
      object Accessor {
        def apply(targetName: Expr[String]): Expr[partial.PathElement.Accessor] =
          exprImpl.PathElementAccessor(targetName)
      }
      object Index {
        def apply(index: Expr[Int]): Expr[partial.PathElement.Index] = exprImpl.PathElementIndex(index)
      }
      object MapKey {
        def apply(key: Expr[Any]): Expr[partial.PathElement.MapKey] = exprImpl.PathElementMapKey(key)
      }
      object MapValue {
        def apply(key: Expr[Any]): Expr[partial.PathElement.MapValue] = exprImpl.PathElementMapValue(key)
      }
    }
  }
  implicit class ExprOps[T: Type](private val expr: Expr[T]) {

    def asInstanceOf[S: Type]: Expr[S] = exprImpl.AsInstanceOf[T, S](expr)
  }

  sealed protected trait DerivedExpr[A]
  protected object DerivedExpr {

    final case class TotalExpr[A](expr: Expr[A]) extends DerivedExpr[A]
    final case class PartialExpr[A](expr: Expr[partial.Result[A]]) extends DerivedExpr[A]
  }

  protected def exprImpl: ExprDefinitionsImpl
  protected trait ExprDefinitionsImpl {

    def Unit: Expr[Unit]

    def Array[A: Type](args: Expr[A]*): Expr[Array[A]]

    def Option[A: Type](value: Expr[A]): Expr[Option[A]]
    def OptionEmpty[A: Type]: Expr[Option[A]]
    def OptionApply[A: Type]: Expr[A => Option[A]]
    def None: Expr[scala.None.type]

    def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]]
    def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]]

    def PartialResultValue[T: Type](value: Expr[T]): Expr[partial.Result.Value[T]]
    def PartialResultErrorsMerge(
        errors1: Expr[partial.Result.Errors],
        errors2: Expr[partial.Result.Errors]
    ): Expr[partial.Result.Errors]
    def PartialResultErrorsMergeResultNullable[T: Type](
        errorsNullable: Expr[partial.Result.Errors],
        result: Expr[partial.Result[T]]
    ): Expr[partial.Result.Errors]
    def PartialResultEmpty[T: Type]: Expr[partial.Result[T]]
    def PartialResultFunction[S: Type, T: Type](f: Expr[S => T]): Expr[S => partial.Result[T]]
    def PartialResultTraverse[M: Type, A: Type, B: Type](
        it: Expr[Iterator[A]],
        f: Expr[A => partial.Result[B]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[M]]
    def PartialResultSequence[M: Type, A: Type](
        it: Expr[Iterator[partial.Result[A]]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[M]]
    def PartialResultMap2[A: Type, B: Type, C: Type](
        fa: Expr[partial.Result[A]],
        fb: Expr[partial.Result[B]],
        f: Expr[(A, B) => C],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[C]]
    def PartialResultProduct[A: Type, B: Type](
        fa: Expr[partial.Result[A]],
        fb: Expr[partial.Result[B]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[(A, B)]]

    def PathElementAccessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor]
    def PathElementIndex(index: Expr[Int]): Expr[partial.PathElement.Index]
    def PathElementMapKey(index: Expr[Any]): Expr[partial.PathElement.MapKey]
    def PathElementMapValue(index: Expr[Any]): Expr[partial.PathElement.MapValue]

    def AsInstanceOf[T: Type, S: Type](expr: Expr[T]): Expr[S]
  }
}
