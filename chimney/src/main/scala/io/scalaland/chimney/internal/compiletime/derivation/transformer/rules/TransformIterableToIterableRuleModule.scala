package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.{Log, MIO}
import hearth.fp.syntax.*
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.Factory

private[compiletime] trait TransformIterableToIterableRuleModule {
  this: Derivation & TransformProductToProductRuleModule & hearth.MacroCommons =>

  import ChimneyType.Implicits.*
  import TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformIterableToIterableRule extends Rule("IterableToIterable") {

    private lazy val Tuple2Ctor: Type.Ctor2[Tuple2] = Type.Ctor2.of[Tuple2]

    // Not implicit, re-exposed as a method-local implicit val where needed (the hearth#316 sibling-implicit-lazy-Type
    // deadlock this guarded against is fixed since 0.4.1 - kept explicit to avoid ambient-implicit ambiguity).
    private lazy val AnyType: Type[Any] = Type.of[Any]

    // Cross-quotes helpers in methods with regular type parameters (the cross-quotes helper-def pattern).

    private def iteratorTypeCompat[A: Type]: Type[Iterator[A]] = Type.of[Iterator[A]]

    private def factoryTypeCompat[A: Type, C: Type]: Type[Factory[A, C]] = Type.of[Factory[A, C]]

    /** Total per-item mapping as foreach+builder (Hearth `IsCollection` mechanics): `{ val f = fn; val b =
      * factory.newBuilder; <if hinted: b.sizeHint(...)>; <foreach src> { item => b += f(item) }; b.result() }` - no
      * `iterator.map` wrapper allocation, and providers with a cheaper traversal (e.g. arrays by index) skip the
      * iterator entirely.
      *
      * `sizeHint` comes from [[TotallyOrPartiallyBuildIterable.builderSizeHint]] (ultimately hearth's per-provider
      * `IsCollectionOf.sizeHintForBuilder`, hearth#354), so it exists ONLY when a non-consuming size expression exists -
      * single-pass sources are `None` by construction (obtaining a size generically would be a second traversal, which
      * emptied java Iterator/Enumeration sources and broke java streams before). The expression may evaluate to a
      * negative value at runtime (unknown size), hence the `>= 0` guard. Array sources still use the single-traversal
      * mapped-iterator encoding in `srcForeachToBuilder` (measured faster than even a hinted builder loop).
      */
    @scala.annotation.nowarn("msg=is never used")
    private def foreachToBuilder[A: Type, B: Type, C: Type](
        fn: Expr[A => B],
        factory: Expr[Factory[B, C]],
        sizeHint: Option[Expr[Int]],
        foreachSrc: (Expr[A] => Expr[Unit]) => Expr[Unit]
    ): Expr[C] = {
      implicit val FnAB: Type[A => B] = Type.of[A => B]
      implicit val FactoryBC: Type[Factory[B, C]] = Type.of[Factory[B, C]]
      implicit val BuilderBC: Type[scala.collection.mutable.Builder[B, C]] =
        Type.of[scala.collection.mutable.Builder[B, C]]
      ValDefs.createVal[A => B](fn, FreshName.FromType).use { fRef =>
        ValDefs
          .createVal[scala.collection.mutable.Builder[B, C]](
            Expr.quote(Expr.splice(factory).newBuilder),
            FreshName.FromType
          )
          .use { bRef =>
            val hint: Expr[Unit] = sizeHint match {
              case Some(size) =>
                // The Int overload of sizeHint (not the IterableOnce one): the latter has a default `delta`
                // argument, and Scala 2 cross-quotes expansion mishandles the default-argument tree.
                Expr.quote {
                  val ks = Expr.splice(size)
                  if (ks >= 0) Expr.splice(bRef).sizeHint(ks)
                }
              case None => Expr.quote(())
            }
            val loop: Expr[Unit] = foreachSrc { (item: Expr[A]) =>
              // suppressUnused (tree-level `val _ = expr; ()`): a quoted `val _`/named-val/bare-statement discard
              // trips (respectively) a Scala 2 reify crash, unused-local warnings, or -Wnonunit-statement.
              Expr.suppressUnused(
                Expr.quote(Expr.splice(bRef).addOne(Expr.splice(fRef).apply(Expr.splice(item))))
              )
            }
            Expr.quote {
              Expr.splice(hint)
              Expr.splice(loop)
              Expr.splice(bRef).result()
            }
          }
      }
    }

    private def iteratorToCompat[A: Type, C: Type](
        it: Expr[Iterator[A]],
        factory: Expr[Factory[A, C]]
    ): Expr[C] = Expr.quote {
      Expr.splice(it).to(Expr.splice(factory))
    }

    private def iteratorZipWithIndexCompat[A: Type](it: Expr[Iterator[A]]): Expr[Iterator[(A, Int)]] = Expr.quote {
      Expr.splice(it).zipWithIndex
    }

    private def iteratorMapCompat[A: Type, B: Type](it: Expr[Iterator[A]], f: Expr[A => B]): Expr[Iterator[B]] = {
      implicit val IteratorB: Type[Iterator[B]] = iteratorTypeCompat[B]
      Expr.quote {
        Expr.splice(it).map(Expr.splice(f))
      }
    }

    private def iteratorConcatCompat[A: Type](
        it: Expr[Iterator[A]],
        it2: Expr[Iterator[A]]
    ): Expr[Iterator[A]] = Expr.quote {
      Expr.splice(it) ++ Expr.splice(it2)
    }

    private def function2TupledCompat[A: Type, B: Type, C: Type](f: Expr[(A, B) => C]): Expr[((A, B)) => C] =
      Expr.quote {
        Expr.splice(f).tupled
      }

    @scala.annotation.nowarn
    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      mapCollections[From, To] match {
        case Right(srcToResult) =>
          lazy val fallbackToResult = mapFallbackCollections[From, To]

          val merge = ctx match {
            case TransformationContext.ForTotal(_)             => mergeTotal[To](_, _)
            case TransformationContext.ForPartial(_, failFast) => mergePartial[To](failFast)(_, _)
          }

          (ctx.config.flags.collectionFallbackMerge match {
            case None                            => srcToResult
            case Some(dsls.SourceAppendFallback) =>
              fallbackToResult
                .foldLeft(srcToResult)(merge)
                .logInfo(s"Combined source collection with ${fallbackToResult.size} fallbacks (appended)")
            case Some(dsls.FallbackAppendSource) =>
              fallbackToResult.reverseIterator
                .foldRight(srcToResult)(merge)
                .logInfo(s"Combined source collection with ${fallbackToResult.size} fallbacks (prepended)")
          }).flatMap(expanded)
        case Left(Some(reason)) => attemptNextRuleBecause(reason)
        case Left(None)         => attemptNextRule
      }

    private def mapCollections[From, To](implicit
        ctx: TransformationContext[From, To]
    ): Either[Option[String], MIO[TransformationExpr[To]]] =
      (ctx, Type[From], Type[To]) match {
        case (
              TransformationContext.ForPartial(_, failFast),
              TotallyOrPartiallyBuildIterable(from2),
              TotallyOrPartiallyBuildIterable(to2)
            ) if from2.value.asMap.isDefined && to2.Underlying.isTuple =>
          val Some((fromK, fromV)) = from2.value.asMap: @unchecked
          val Tuple2Ctor(toK, toV) = to2.Underlying: @unchecked
          import fromK.Underlying as FromK, fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          Right(
            Log.info(
              s"Resolved ${Type.prettyPrint[From]} (${from2.value}) as map type and ${Type.prettyPrint[To]} (${to2.value}) as iterable of tuple"
            ) >>
              mapPartialMaps[From, To, FromK, FromV, ToK, ToV](
                from2.value.asInstanceOf[TotallyOrPartiallyBuildIterable[From, (FromK, FromV)]],
                to2.value.asInstanceOf[TotallyOrPartiallyBuildIterable[To, (ToK, ToV)]],
                failFast
              )
          )
        case (TransformationContext.ForTotal(_), TotallyOrPartiallyBuildIterable(_), PartiallyBuildIterable(to2)) =>
          Left(
            Some(
              s"Only PartiallyBuildIterable available for ${Type.prettyPrint[To]} (${to2.value}), in total context"
            )
          )
        case (_, TotallyOrPartiallyBuildIterable(from2), TotallyOrPartiallyBuildIterable(to2)) =>
          import from2.{Underlying as InnerFrom, value as fromIterable},
            to2.{Underlying as InnerTo, value as toIterable}
          Right(
            Log.info(
              s"Resolved ${Type.prettyPrint[From]} (${from2.value}) and ${Type.prettyPrint[To]} (${to2.value}) as iterable types"
            ) >>
              mapIterables[From, To, InnerFrom, InnerTo](fromIterable, toIterable)
          )
        case _ => Left(None)
      }

    private def mapFallbackCollections[From, To](implicit
        ctx: TransformationContext[From, To]
    ): Vector[MIO[TransformationExpr[To]]] =
      ctx.config.filterCurrentOverridesForFallbacks.view
        .map { case TransformerOverride.Fallback(fallback) =>
          import fallback.{Underlying as Fallback, value as fallbackExpr}
          implicit val iterableCtx: TransformationContext[Fallback, To] =
            ctx.updateFromTo[Fallback, To](fallbackExpr, updateFallbacks = _ => Vector.empty)(using Fallback, ctx.To)
          mapCollections[Fallback, To]
        }
        .collect { case Right(value) => value }
        .toVector

    private def mapPartialMaps[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        fromIterable: TotallyOrPartiallyBuildIterable[From, (FromK, FromV)],
        toIterable: TotallyOrPartiallyBuildIterable[To, (ToK, ToV)],
        failFast: Expr[Boolean]
    )(implicit ctx: TransformationContext[From, To]): MIO[TransformationExpr[To]] = {
      implicit val AnyT: Type[Any] = AnyType
      implicit val TupleFromKV: Type[(FromK, FromV)] = Type.of[(FromK, FromV)]
      implicit val TupleToKV: Type[(ToK, ToV)] = Type.of[(ToK, ToV)]
      LambdaBuilder
        .of2[FromK, FromV](FreshName.FromPrefix("key"), FreshName.FromPrefix("value"))
        .traverse[MIO, ((Expr[partial.Result[ToK]], Expr[FromK]), Expr[partial.Result[ToV]])] { case (key, value) =>
          val toKeyResult = Log
            .namedScope("Derive Map's key mapping") {
              useOverrideIfPresentOr("everyMapKey", ctx.config.filterCurrentOverridesForEveryMapKey) {
                deriveRecursiveTransformationExpr[FromK, ToK](
                  key,
                  followFrom = Path(_.everyMapKey),
                  followTo = Path(_.everyMapKey),
                  updateFallbacks = _ => Vector.empty
                )
              }
            }
            .map(_.ensurePartial -> key)
          val toValueResult = Log
            .namedScope("Derive Map's value mapping") {
              useOverrideIfPresentOr("everyMapValue", ctx.config.filterCurrentOverridesForEveryMapValue) {
                deriveRecursiveTransformationExpr[FromV, ToV](
                  value,
                  followFrom = Path(_.everyMapValue),
                  followTo = Path(_.everyMapValue),
                  updateFallbacks = _ => Vector.empty
                )
              }
            }
            .map(_.ensurePartial)
          toKeyResult.parTuple(toValueResult)
        }
        .flatMap { builder =>
          def partialResultTraverse[ToOrPartialTo: Type](
              factory: Expr[Factory[(ToK, ToV), ToOrPartialTo]]
          ): Expr[partial.Result[ToOrPartialTo]] =
            // We're constructing:
            // '{ partial.Result.traverse(
            //   ${ src }.iterator,
            //   { case (fromKey, fromValue) =>
            //     partial.Result.product(
            //       ${ derivedToKey }.unsealErrorPath.prependErrorPath(partial.PathElement.MapKey(fromKey)))
            //       ${ derivedToValue }.unsealErrorPath.prependErrorPath(partial.PathElement.MapValue(fromKey))),
            //   },
            //   ${ failFast }
            // )(${ factory }) }
            ChimneyExpr.PartialResult
              .traverse[ToOrPartialTo, (FromK, FromV), (ToK, ToV)](
                fromIterable.iterator(ctx.src),
                function2TupledCompat(builder.buildWith { case ((keyResult, key), valueResult) =>
                  ChimneyExpr.PartialResult.product(
                    keyResult.unsealErrorPath.prependErrorPath(
                      ChimneyExpr.PathElement.MapKey(key.upcast[Any]).upcast[partial.PathElement]
                    ),
                    valueResult.unsealErrorPath.prependErrorPath(
                      ChimneyExpr.PathElement.MapValue(key.upcast[Any]).upcast[partial.PathElement]
                    ),
                    failFast
                  )
                }),
                failFast,
                factory
              )

          toIterable.factory match {
            case Left(totalFactory)    => partialExpr(partialResultTraverse(totalFactory))
            case Right(partialFactory) => partialExpr(partialResultTraverse(partialFactory).flatten)
          }
        }
    }

    private def mapIterables[From, To, InnerFrom: Type, InnerTo: Type](
        fromIterable: TotallyOrPartiallyBuildIterable[From, InnerFrom],
        toIterable: TotallyOrPartiallyBuildIterable[To, InnerTo]
    )(implicit ctx: TransformationContext[From, To]): MIO[TransformationExpr[To]] = {
      implicit val PairInnerFromInt: Type[(InnerFrom, Int)] = Type.of[(InnerFrom, Int)]
      LambdaBuilder
        .of1[InnerFrom]()
        .traverse[MIO, TransformationExpr[InnerTo]] { (newFromSrc: Expr[InnerFrom]) =>
          useOverrideIfPresentOr("everyItem", ctx.config.filterCurrentOverridesForEveryItem) {
            Log.namedScope("Derive collection's item mapping") {
              deriveRecursiveTransformationExpr[InnerFrom, InnerTo](
                newFromSrc,
                followFrom = Path(_.everyItem),
                followTo = Path(_.everyItem),
                updateFallbacks = _ => Vector.empty
              )
            }
          }
        }
        .flatMap { (to2P: LambdaBuilder[InnerFrom => *, TransformationExpr[InnerTo]]) =>
          to2P.foldTransformationExpr { (totalP: LambdaBuilder[InnerFrom => *, Expr[InnerTo]]) =>
            // TODO: restore .map implementation
            if (Type[InnerFrom] =:= Type[InnerTo] && ctx.config.areOverridesEmpty) {
              def srcToFactory[ToOrPartialTo: Type](
                  factory: Expr[Factory[InnerTo, ToOrPartialTo]]
              ): Expr[ToOrPartialTo] = {
                // helper defs, not inline Type.of: cross-quotes inside a def nested in lambdas does not substitute
                // the type parameters on Scala 2
                implicit val FactoryInnerFromType: Type[Factory[InnerFrom, ToOrPartialTo]] =
                  factoryTypeCompat[InnerFrom, ToOrPartialTo]
                implicit val FactoryInnerToType: Type[Factory[InnerTo, ToOrPartialTo]] =
                  factoryTypeCompat[InnerTo, ToOrPartialTo]
                // We're constructing:
                // '{ ${ src }.to(Factory[$InnerTo, $ToOrPartialTo]) }
                fromIterable.to[ToOrPartialTo](ctx.src, factory.upcast[Factory[InnerFrom, ToOrPartialTo]])
              }

              toIterable.factory match {
                case Left(totalFactory)    => totalExpr(srcToFactory(totalFactory))
                case Right(partialFactory) => partialExpr(srcToFactory(partialFactory))
              }
            } else {
              def srcForeachToBuilder[ToOrPartialTo: Type](
                  factory: Expr[Factory[InnerTo, ToOrPartialTo]]
              ): Expr[ToOrPartialTo] =
                if ((ctx.From: Type[From]).isArray)
                  // Array sources use the mapped-iterator encoding: `iterator.map(f).to(factory)` fills the target
                  // builder through `addAll`'s monomorphic inner loop with a knownSize pre-allocation, which measures
                  // at by-hand speed for Array -> Array (the foreach+builder encoding measured 0.55x there: builder
                  // regrowth + an extra `result()` copy; even size-hinted it stayed ~18% behind on the call-site
                  // interface `addOne` per element). Single traversal, so single-pass sources are unaffected - and
                  // arrays are multi-pass anyway.
                  iteratorToCompat[InnerTo, ToOrPartialTo](
                    iteratorMapCompat[InnerFrom, InnerTo](fromIterable.iterator(ctx.src), totalP.build[InnerTo]),
                    factory
                  )
                else
                  // We're constructing
                  // '{ val f = from2 => ${ derivedInnerTo }; val b = ${ factory }.newBuilder
                  //    <if the provider has a safe size: b.sizeHint(...)>
                  //    <foreach over src> { item => b += f(item) }; b.result() }
                  foreachToBuilder[InnerFrom, InnerTo, ToOrPartialTo](
                    totalP.build[InnerTo],
                    factory,
                    fromIterable.builderSizeHint(ctx.src),
                    f => fromIterable.foreach(ctx.src)(f)
                  )

              toIterable.factory match {
                case Left(totalFactory)    => totalExpr(srcForeachToBuilder(totalFactory))
                case Right(partialFactory) => partialExpr(srcForeachToBuilder(partialFactory))
              }
            }
          } { (partialP: LambdaBuilder[InnerFrom => *, Expr[partial.Result[InnerTo]]]) =>
            ctx match {
              case TransformationContext.ForPartial(src, failFast) =>
                def partialResultTraverse[ToOrPartialTo: Type](
                    factory: Expr[Factory[InnerTo, ToOrPartialTo]]
                ): Expr[partial.Result[ToOrPartialTo]] =
                  // We're constructing:
                  // '{ partial.Result.traverse[To, ($InnerFrom, Int), $InnerTo](
                  //   ${ src }.iterator.zipWithIndex,
                  //   { pair =>
                  //     ${ innerLambda }(pair._1).unsealErrorPath.prependErrorPath(partial.PathElement.Index(pair._2))
                  //   },
                  //   ${ failFast }
                  // )(${ factory }) }
                  ChimneyExpr.PartialResult.traverse[ToOrPartialTo, (InnerFrom, Int), InnerTo](
                    iteratorZipWithIndexCompat(fromIterable.iterator(src)),
                    indexedPartialLambda[InnerFrom, InnerTo](partialP.build[partial.Result[InnerTo]]),
                    failFast,
                    factory
                  )

                toIterable.factory match {
                  case Left(totalTransformer) =>
                    partialExpr(partialResultTraverse(totalTransformer))
                  case Right(partialTransformer) =>
                    partialExpr(partialResultTraverse(partialTransformer).flatten)
                }
              case TransformationContext.ForTotal(_) =>
                MIO.fail(new AssertionError("Derived Partial Expr for Total Context"))
            }
          }
        }
    }

    /** Wraps the derived per-item partial lambda into the `((InnerFrom, Int)) => partial.Result[InnerTo]` shape that
      * `partial.Result.traverse` over `iterator.zipWithIndex` expects.
      */
    private def indexedPartialLambda[InnerFrom: Type, InnerTo: Type](
        inner: Expr[InnerFrom => partial.Result[InnerTo]]
    ): Expr[((InnerFrom, Int)) => partial.Result[InnerTo]] = {
      implicit val FnType: Type[InnerFrom => partial.Result[InnerTo]] =
        Type.of[InnerFrom => partial.Result[InnerTo]]
      ValDefs.createVal(inner, FreshName.FromPrefix("inner")).use { innerRef =>
        Expr.quote { (pair: (InnerFrom, Int)) =>
          Expr
            .splice(innerRef)
            .apply(pair._1)
            .unsealErrorPath
            .prependErrorPath(io.scalaland.chimney.partial.PathElement.Index(pair._2))
        }
      }
    }

    // Exposed for TransformMapToMapRuleModule

    def mergeTotal[To: Type](
        result1: MIO[TransformationExpr[To]],
        result2: MIO[TransformationExpr[To]]
    ): MIO[TransformationExpr[To]] = result1.map2(result2)(mergeTotalExprs[To])

    def mergeTotalExprs[To: Type](
        texpr1: TransformationExpr[To],
        texpr2: TransformationExpr[To]
    ): TransformationExpr[To] = {
      val TotallyBuildIterable(to2) = Type[To]: @unchecked
      import to2.{Underlying as InnerTo, value as buildIterable}
      TransformationExpr.fromTotal(
        iteratorToCompat(
          iteratorConcatCompat(
            buildIterable.iterator(texpr1.ensureTotal),
            buildIterable.iterator(texpr2.ensureTotal)
          ),
          buildIterable.totalFactory
        )
      )
    }

    def mergePartial[To: Type](failFast: Expr[Boolean])(
        result1: MIO[TransformationExpr[To]],
        result2: MIO[TransformationExpr[To]]
    ): MIO[TransformationExpr[To]] =
      result1.map2(result2)(mergePartialExprs[To](failFast))

    def mergePartialExprs[To: Type](failFast: Expr[Boolean])(
        texpr1: TransformationExpr[To],
        texpr2: TransformationExpr[To]
    ): TransformationExpr[To] = {
      val TotallyOrPartiallyBuildIterable(to2) = Type[To]: @unchecked
      import to2.{Underlying as InnerTo, value as buildIterable}
      mergePartialExprsImpl[To, InnerTo](buildIterable, failFast, texpr1, texpr2)
    }

    private def mergePartialExprsImpl[To: Type, InnerTo: Type](
        buildIterable: TotallyOrPartiallyBuildIterable[To, InnerTo],
        failFast: Expr[Boolean],
        texpr1: TransformationExpr[To],
        texpr2: TransformationExpr[To]
    ): TransformationExpr[To] = {
      implicit val IteratorInnerTo: Type[Iterator[InnerTo]] = iteratorTypeCompat[InnerTo]

      val iterators: TransformationExpr[Iterator[InnerTo]] = (texpr1, texpr2) match {
        case (TransformationExpr.TotalExpr(expr1), TransformationExpr.TotalExpr(expr2)) =>
          TransformationExpr.fromTotal[Iterator[InnerTo]](
            iteratorConcatCompat(buildIterable.iterator(expr1), buildIterable.iterator(expr2))
          )
        case _ =>
          TransformationExpr.fromPartial[Iterator[InnerTo]](
            texpr1.ensurePartial.map2(texpr2.ensurePartial, failFast)(
              LambdaBuilder.of2[To, To]().buildWith { case (expr1, expr2) =>
                iteratorConcatCompat(buildIterable.iterator(expr1), buildIterable.iterator(expr2))
              }
            )
          )
      }
      iterators.flatMap { expr =>
        buildIterable.factory match {
          case Left(totalFactor)     => TransformationExpr.fromTotal(iteratorToCompat(expr, totalFactor))
          case Right(partialFactory) => TransformationExpr.fromPartial(iteratorToCompat(expr, partialFactory))
        }
      }
    }
  }
}
