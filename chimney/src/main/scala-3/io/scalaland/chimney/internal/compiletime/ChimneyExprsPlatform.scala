package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.{integrations, partial, PartialTransformer, Patcher, Transformer}

import scala.collection.compat.Factory
import scala.quoted

private[compiletime] trait ChimneyExprsPlatform extends ChimneyExprs { this: ChimneyDefinitionsPlatform =>

  object ChimneyExpr extends ChimneyExprModule {

    import Expr.platformSpecific.resetOwner

    object Transformer extends TransformerModule {

      def transform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.Transformer[From, To]],
          src: Expr[From]
      ): Expr[To] = '{ ${ transformer }.transform(${ resetOwner(src) }) }

      def instance[From: Type, To: Type](toExpr: Expr[From] => Expr[To]): Expr[Transformer[From, To]] =
        '{
          new Transformer[From, To] {
            def transform(src: From): To = ${
              PrependDefinitionsTo
                .prependVal[From](resetOwner('{ src }), ExprPromise.NameGenerationStrategy.FromType)
                .use(toExpr)
            }
          }
        }
    }

    object PartialTransformer extends PartialTransformerModule {

      def transform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.PartialTransformer[From, To]],
          src: Expr[From],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[To]] =
        '{ ${ transformer }.transform(${ resetOwner(src) }, ${ resetOwner(failFast) }) }

      def instance[From: Type, To: Type](
          toExpr: (Expr[From], Expr[Boolean]) => Expr[partial.Result[To]]
      ): Expr[PartialTransformer[From, To]] =
        '{
          new PartialTransformer[From, To] {
            def transform(src: From, failFast: Boolean): partial.Result[To] = ${
              PrependDefinitionsTo
                .prependVal[From](resetOwner('{ src }), ExprPromise.NameGenerationStrategy.FromType)
                .use(toExpr(_, resetOwner('{ failFast })))
            }
          }
        }
    }

    object PartialResult extends PartialResultModule {
      object Value extends ValueModule {
        def apply[A: Type](value: Expr[A]): Expr[partial.Result.Value[A]] =
          '{ partial.Result.Value[A](${ value }) }

        def value[A: Type](valueExpr: Expr[partial.Result.Value[A]]): Expr[A] =
          '{ ${ valueExpr }.value }
      }

      object Errors extends ErrorsModule {
        def merge(
            errors1: Expr[partial.Result.Errors],
            errors2: Expr[partial.Result.Errors]
        ): Expr[partial.Result.Errors] =
          '{ partial.Result.Errors.merge(${ errors1 }, ${ errors2 }) }

        def mergeResultNullable[A: Type](
            errorsNullable: Expr[partial.Result.Errors],
            result: Expr[partial.Result[A]]
        ): Expr[partial.Result.Errors] =
          '{ partial.Result.Errors.__mergeResultNullable[A](${ errorsNullable }, ${ result }) }
      }

      def fromEmpty[A: Type]: Expr[partial.Result[A]] = '{ partial.Result.fromEmpty[A] }

      def fromFunction[A: Type, B: Type](f: Expr[A => B]): Expr[A => partial.Result[B]] =
        '{ partial.Result.fromFunction[A, B](${ f }) }

      def traverse[M: Type, A: Type, B: Type](
          it: Expr[Iterator[A]],
          f: Expr[A => partial.Result[B]],
          failFast: Expr[Boolean],
          factory: Expr[Factory[B, M]]
      ): Expr[partial.Result[M]] =
        '{
          partial.Result.traverse[M, A, B](${ resetOwner(it) }, ${ resetOwner(f) }, ${ resetOwner(failFast) })(${
            factory
          })
        }

      def sequence[M: Type, A: Type](
          it: Expr[Iterator[partial.Result[A]]],
          failFast: Expr[Boolean],
          factory: Expr[Factory[A, M]]
      ): Expr[partial.Result[M]] =
        '{ partial.Result.sequence[M, A](${ it }, ${ failFast })(${ factory }) }

      def flatMap[A: Type, B: Type](pr: Expr[partial.Result[A]])(
          f: Expr[A => partial.Result[B]]
      ): Expr[partial.Result[B]] =
        '{ ${ pr }.flatMap(${ f }) }

      def flatten[A: Type](pr: Expr[partial.Result[partial.Result[A]]]): Expr[partial.Result[A]] =
        '{ ${ pr }.flatten[A] }

      def map[A: Type, B: Type](pr: Expr[partial.Result[A]])(f: Expr[A => B]): Expr[partial.Result[B]] =
        '{ ${ pr }.map(${ f }) }

      def map2[A: Type, B: Type, C: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          f: Expr[(A, B) => C],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[C]] =
        '{
          partial.Result.map2[A, B, C](
            ${ resetOwner(fa) },
            ${ resetOwner(fb) },
            ${ resetOwner(f) },
            ${ resetOwner(failFast) }
          )
        }

      def product[A: Type, B: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[(A, B)]] =
        '{ partial.Result.product[A, B](${ resetOwner(fa) }, ${ resetOwner(fb) }, ${ resetOwner(failFast) }) }

      def prependErrorPath[A: Type](
          fa: Expr[partial.Result[A]],
          path: Expr[partial.PathElement]
      ): Expr[partial.Result[A]] =
        '{ ${ fa }.prependErrorPath(${ path }) }
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

    object RuntimeDataStore extends RuntimeDataStoreModule {

      val empty: Expr[TransformerDefinitionCommons.RuntimeDataStore] =
        '{ TransformerDefinitionCommons.emptyRuntimeDataStore }

      def extractAt(
          runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore],
          index: Int
      ): Expr[Any] =
        '{ ${ runtimeDataStore }.apply(${ quoted.Expr(index) }) }
    }

    object Patcher extends PatcherModule {

      def patch[A: Type, Patch: Type](
          patcher: Expr[io.scalaland.chimney.Patcher[A, Patch]],
          obj: Expr[A],
          patch: Expr[Patch]
      ): Expr[A] = '{
        ${ patcher }.patch(${ obj }, ${ patch })
      }

      def instance[A: Type, Patch: Type](
          f: (Expr[A], Expr[Patch]) => Expr[A]
      ): Expr[io.scalaland.chimney.Patcher[A, Patch]] =
        '{
          new Patcher[A, Patch] {
            def patch(obj: A, patch: Patch): A = ${
              PrependDefinitionsTo
                .prependVal[A](resetOwner('{ obj }), ExprPromise.NameGenerationStrategy.FromType)
                .map2(
                  PrependDefinitionsTo
                    .prependVal[Patch](resetOwner('{ patch }), ExprPromise.NameGenerationStrategy.FromType)
                )(
                  f
                )
                .closeBlockAsExprOf[A]
            }
          }
        }
    }

    object OptionalValue extends OptionalValueModule {

      def empty[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]]
      ): Expr[Optional] = '{ ${ optionalValue }.empty }

      def of[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          value: Expr[Value]
      ): Expr[Optional] = '{ ${ optionalValue }.of(${ value }) }

      def fold[Optional: Type, Value: Type, A: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          onNone: Expr[A],
          onSome: Expr[Value => A]
      ): Expr[A] = '{ ${ optionalValue }.fold(${ optional }, ${ onNone }, ${ onSome }) }

      def getOrElse[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          onNone: Expr[Value]
      ): Expr[Value] = '{ ${ optionalValue }.getOrElse(${ optional }, ${ onNone }) }

      def orElse[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          optional2: Expr[Optional]
      ): Expr[Optional] = '{ ${ optionalValue }.orElse(${ optional }, ${ optional2 }) }
    }
  }
}
