package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait ChimneyExprs { this: Definitions =>

  val ChimneyExpr: ChimneyExprModule
  trait ChimneyExprModule { this: ChimneyExpr.type =>

    val PartialResult: PartialResultModule
    trait PartialResultModule { this: PartialResult.type =>

      def Value[T: Type](value: Expr[T]): Expr[partial.Result.Value[T]]

      val Errors: ErrorsModule

      trait ErrorsModule { this: Errors.type =>

        def merge(
            errors1: Expr[partial.Result.Errors],
            errors2: Expr[partial.Result.Errors]
        ): Expr[partial.Result.Errors]

        def mergeResultNullable[T: Type](
            errorsNullable: Expr[partial.Result.Errors],
            result: Expr[partial.Result[T]]
        ): Expr[partial.Result.Errors]
      }

      def fromEmpty[T: Type]: Expr[partial.Result[T]]

      def fromFunction[S: Type, T: Type](f: Expr[S => T]): Expr[S => partial.Result[T]]

      def traverse[M: Type, A: Type, B: Type](
          it: Expr[Iterator[A]],
          f: Expr[A => partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[M]]

      def sequence[M: Type, A: Type](
          it: Expr[Iterator[partial.Result[A]]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[M]]

      def map2[A: Type, B: Type, C: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          f: Expr[(A, B) => C],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[C]]

      def product[A: Type, B: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[(A, B)]]
    }

    val PathElement: PathElementModule
    trait PathElementModule { this: PathElement.type =>
      def Accessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor]
      def Index(index: Expr[Int]): Expr[partial.PathElement.Index]
      def MapKey(key: Expr[Any]): Expr[partial.PathElement.MapKey]
      def MapValue(key: Expr[Any]): Expr[partial.PathElement.MapValue]
    }
  }

  // TODO: move to a separate, higher level models module?
  // TODO: rename to TransformerBodyExpr?
  sealed protected trait DerivedExpr[A]
  protected object DerivedExpr {
    final case class TotalExpr[A](expr: Expr[A]) extends DerivedExpr[A]
    final case class PartialExpr[A](expr: Expr[partial.Result[A]]) extends DerivedExpr[A]
  }
}
