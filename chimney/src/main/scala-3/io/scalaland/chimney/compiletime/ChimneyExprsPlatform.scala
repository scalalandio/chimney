package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}

import scala.collection.compat.Factory
import scala.quoted

private[compiletime] trait ChimneyExprsPlatform extends ChimneyExprs { this: DefinitionsPlatform =>

  object ChimneyExpr extends ChimneyExprModule {

    object PartialResult extends PartialResultModule {
      def Value[T: Type](value: Expr[T]): Expr[partial.Result.Value[T]] =
        '{ partial.Result.Value[T](${ value }) }

      object Errors extends ErrorsModule {
        def merge(
            errors1: Expr[partial.Result.Errors],
            errors2: Expr[partial.Result.Errors]
        ): Expr[partial.Result.Errors] =
          '{ partial.Result.Errors.merge(${ errors1 }, ${ errors2 }) }

        def mergeResultNullable[T: Type](
            errorsNullable: Expr[partial.Result.Errors],
            result: Expr[partial.Result[T]]
        ): Expr[partial.Result.Errors] =
          '{ partial.Result.Errors.__mergeResultNullable[T](${ errorsNullable }, ${ result }) }
      }

      def fromEmpty[T: Type]: Expr[partial.Result[T]] = '{ partial.Result.fromEmpty[T] }

      def fromFunction[S: Type, T: Type](f: Expr[S => T]): Expr[S => partial.Result[T]] =
        '{ partial.Result.fromFunction[S, T](${ f }) }

      def traverse[M: Type, A: Type, B: Type](
          it: Expr[Iterator[A]],
          f: Expr[A => partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[M]] =
        '{
          partial.Result.traverse[M, A, B](${ it }, ${ f }, ${ failFast })(${ quoted.Expr.summon[Factory[B, M]].get })
        }

      def sequence[M: Type, A: Type](
          it: Expr[Iterator[partial.Result[A]]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[M]] =
        '{ partial.Result.sequence[M, A](${ it }, ${ failFast })(${ quoted.Expr.summon[Factory[A, M]].get }) }

      def map2[A: Type, B: Type, C: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          f: Expr[(A, B) => C],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[C]] =
        '{ partial.Result.map2[A, B, C](${ fa }, ${ fb }, ${ f }, ${ failFast }) }

      def product[A: Type, B: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[(A, B)]] =
        '{ partial.Result.product[A, B](${ fa }, ${ fb }, ${ failFast }) }
    }

    object PathElement extends PathElementModule {
      def Accessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor] =
        '{ partial.PathElement.Accessor(${ targetName }) }

      def Index(index: Expr[Int]): Expr[partial.PathElement.Index] =
        '{ partial.PathElement.Index(${ index }) }

      def MapKey(key: Expr[Any]): Expr[partial.PathElement.MapKey] =
        '{ partial.PathElement.MapKey(${ key }) }

      def MapValue(key: Expr[Any]): Expr[partial.PathElement.MapValue] =
        '{ partial.PathElement.MapValue(${ key }) }
    }
  }
}
