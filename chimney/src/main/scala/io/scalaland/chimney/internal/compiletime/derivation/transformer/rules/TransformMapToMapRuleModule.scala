package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.{Log, MIO}
import hearth.fp.syntax.*
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.Factory

/** `MIO` is lazy, so `mapMaps` cannot eagerly run the delegated-to-IterableToIterable expansion to decide whether to
  * pass on to the next rule - instead it returns `MIO[Either[Option[String], TransformationExpr[To]]]`: `Left(reason)`
  * is the decided-at-derivation-time "attempt next rule" of the delegated expansion; `expand` translates a `Left` of
  * the main result into `AttemptNextRule`, and fallbacks that delegated-and-yielded are skipped during the merge fold.
  */
private[compiletime] trait TransformMapToMapRuleModule {
  this: Derivation & TransformIterableToIterableRuleModule & TransformProductToProductRuleModule &
    hearth.MacroCommons =>

  import ChimneyType.Implicits.*
  import TransformIterableToIterableRule.{mergePartialExprs, mergeTotalExprs}
  import TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformMapToMapRule extends Rule("MapToMap") {

    private lazy val Tuple2Ctor: Type.Ctor2[Tuple2] = Type.Ctor2.of[Tuple2]

    // Not implicit, re-exposed as a method-local implicit val where needed (the hearth#316 sibling-implicit-lazy-Type
    // deadlock this guarded against is fixed since 0.4.1 - kept explicit to avoid ambient-implicit ambiguity).
    private lazy val AnyType: Type[Any] = Type.of[Any]

    // Cross-quotes helpers in methods with regular type parameters (the cross-quotes helper-def pattern).

    private def tuple2TypeCompat[A: Type, B: Type]: Type[(A, B)] = Type.of[(A, B)]

    private def iteratorTypeCompat[A: Type]: Type[Iterator[A]] = Type.of[Iterator[A]]

    private def factoryTypeCompat[A: Type, C: Type]: Type[Factory[A, C]] = Type.of[Factory[A, C]]

    // Upcast hoisted into a helper def: local implicit vals whose types mention pattern-extracted existentials
    // trip Scala 2's "recursive value needs type" cycle detection.
    private def upcastTupleIteratorCompat[A: Type, K: Type, V: Type](it: Expr[Iterator[A]]): Expr[Iterator[(K, V)]] = {
      implicit val IteratorAType: Type[Iterator[A]] = iteratorTypeCompat[A]
      implicit val TupleKVType: Type[(K, V)] = tuple2TypeCompat[K, V]
      implicit val IteratorKVType: Type[Iterator[(K, V)]] = iteratorTypeCompat[(K, V)]
      it.upcast[Iterator[(K, V)]]
    }

    private def tuple2ExprCompat[A: Type, B: Type](a: Expr[A], b: Expr[B]): Expr[(A, B)] = {
      implicit val TupleAB: Type[(A, B)] = Type.of[(A, B)]
      Expr.quote((Expr.splice(a), Expr.splice(b)))
    }

    private def iteratorMapCompat[A: Type, B: Type](
        it: Expr[Iterator[A]],
        f: Expr[A => B]
    ): Expr[Iterator[B]] = Expr.quote {
      Expr.splice(it).map(Expr.splice(f))
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

    private def function2TupledCompat[A: Type, B: Type, C: Type](f: Expr[(A, B) => C]): Expr[((A, B)) => C] =
      Expr.quote {
        Expr.splice(f).tupled
      }

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      mapMaps[From, To] match {
        case Right(srcToResult) =>
          srcToResult.flatMap {
            case Left(reason) =>
              // The delegated IterableToIterable expansion decided to pass on to the next rule - so does MapToMap.
              MIO.pure(Rule.ExpansionResult.AttemptNextRule(reason))
            case Right(srcTo) =>
              lazy val fallbackToResult: Vector[MIO[Option[TransformationExpr[To]]]] =
                mapFallbackMaps[From, To].map(_.map(_.toOption))

              val merge: (TransformationExpr[To], TransformationExpr[To]) => TransformationExpr[To] = ctx match {
                case TransformationContext.ForTotal(_)             => mergeTotalExprs[To](_, _)
                case TransformationContext.ForPartial(_, failFast) => mergePartialExprs[To](failFast)(_, _)
              }

              (ctx.config.flags.collectionFallbackMerge match {
                case None                            => MIO.pure(srcTo)
                case Some(dsls.SourceAppendFallback) =>
                  fallbackToResult
                    .foldLeft(MIO.pure(srcTo)) { (acc, fallbackOpt) =>
                      acc.map2(fallbackOpt)((a, opt) => opt.fold(a)(b => merge(a, b)))
                    }
                    .logInfo(s"Combined source Map with ${fallbackToResult.size} fallbacks (appended)")
                case Some(dsls.FallbackAppendSource) =>
                  fallbackToResult.reverseIterator
                    .foldRight(MIO.pure(srcTo)) { (fallbackOpt, acc) =>
                      fallbackOpt.map2(acc)((opt, a) => opt.fold(a)(b => merge(b, a)))
                    }
                    .logInfo(s"Combined source Map with ${fallbackToResult.size} fallbacks (prepended)")
              }).flatMap(expanded)
          }
        case Left(Some(reason)) => attemptNextRuleBecause(reason)
        case Left(None)         => attemptNextRule
      }

    private def mapMaps[From, To](implicit
        ctx: TransformationContext[From, To]
    ): Either[Option[String], MIO[Either[Option[String], TransformationExpr[To]]]] =
      (Type[From], Type[To]) match {
        case (TotallyOrPartiallyBuildMap(fromMap), TotallyOrPartiallyBuildMap(toMap)) =>
          import fromMap.{Key as FromK, Value as FromV}, toMap.{Key as ToK, Value as ToV}
          ctx match {
            case TransformationContext.ForTotal(_) if !ctx.config.areOverridesEmpty =>
              Right(
                mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](fromMap.iterator(ctx.src), toMap.factory)
                  .map(Right(_))
              )
            case TransformationContext.ForPartial(_, failFast) =>
              Right(
                mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
                  fromMap.iterator(ctx.src),
                  toMap.factory,
                  failFast,
                  isConversionFromMap = true
                ).map(Right(_))
              )
            case _ =>
              Right(
                Log
                  .namedScope(
                    "MapToMap matched in the context of total transformation without overrides - delegating to IterableToIterable (fallbacks handled in MapToMap)"
                  ) {
                    // Removes fallbacks, as are they are handled here (otherwise they would be appended/prepended twice)
                    TransformIterableToIterableRule.expand(
                      ctx.updateFromTo[From, To](ctx.src, updateFallbacks = _ => Vector.empty)(using ctx.From, ctx.To)
                    )
                  }
                  .map {
                    case Rule.ExpansionResult.Expanded(texpr) =>
                      Right(texpr.asInstanceOf[TransformationExpr[To]])
                    case Rule.ExpansionResult.AttemptNextRule(reason) =>
                      Left(reason)
                  }
              )
          }
        case (TotallyOrPartiallyBuildIterable(from2), TotallyOrPartiallyBuildMap(toMap))
            if !ctx.config.areOverridesEmpty && from2.Underlying.isTuple =>
          val Tuple2Ctor(fromK, fromV) = from2.Underlying: @unchecked
          import from2.{Underlying as InnerFrom, value as fromIterable}, fromK.Underlying as FromK,
            fromV.Underlying as FromV, toMap.{Key as ToK, Value as ToV}
          ctx match {
            case TransformationContext.ForTotal(_) =>
              Right(
                mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](
                  // upcast needed because iterable, not map
                  upcastTupleIteratorCompat[InnerFrom, FromK, FromV](fromIterable.iterator(ctx.src)),
                  toMap.factory
                ).map(Right(_))
              )
            case TransformationContext.ForPartial(_, failFast) =>
              Right(
                mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
                  upcastTupleIteratorCompat[InnerFrom, FromK, FromV](fromIterable.iterator(ctx.src)),
                  toMap.factory,
                  failFast,
                  isConversionFromMap = false
                ).map(Right(_))
              )
          }

        case _ => Left(None)
      }

    private def mapFallbackMaps[From, To](implicit
        ctx: TransformationContext[From, To]
    ): Vector[MIO[Either[Option[String], TransformationExpr[To]]]] =
      ctx.config.filterCurrentOverridesForFallbacks.view
        .map { case TransformerOverride.Fallback(fallback) =>
          import fallback.{Underlying as Fallback, value as fallbackExpr}
          implicit val iterableCtx: TransformationContext[Fallback, To] =
            ctx.updateFromTo[Fallback, To](fallbackExpr, updateFallbacks = _ => Vector.empty)(using Fallback, ctx.To)
          val x = mapMaps[Fallback, To]
          if (ctx.config.flags.displayMacrosLogging) {
            println(s"Fallbacks: ${ctx.config.filterCurrentOverridesForFallbacks}\nHandled as: $x\n")
          }
          x
        }
        .collect { case Right(value) => value }
        .toVector

    private def deriveKeyMapping[From, To, FromK: Type, ToK: Type](
        key: Expr[FromK]
    )(implicit ctx: TransformationContext[From, To]): MIO[TransformationExpr[ToK]] =
      Log.namedScope("Derive Map's key mapping") {
        useOverrideIfPresentOr("everyMapKey", ctx.config.filterCurrentOverridesForEveryMapKey) {
          deriveRecursiveTransformationExpr[FromK, ToK](
            key,
            followFrom = Path(_.everyMapKey),
            followTo = Path(_.everyMapKey),
            updateFallbacks = _ => Vector.empty
          )
        }
      }

    private def deriveValueMapping[From, To, FromV: Type, ToV: Type](
        value: Expr[FromV]
    )(implicit ctx: TransformationContext[From, To]): MIO[TransformationExpr[ToV]] =
      Log.namedScope("Derive Map's value mapping") {
        useOverrideIfPresentOr("everyMapValue", ctx.config.filterCurrentOverridesForEveryMapValue) {
          deriveRecursiveTransformationExpr[FromV, ToV](
            value,
            followFrom = Path(_.everyMapValue),
            followTo = Path(_.everyMapValue),
            updateFallbacks = _ => Vector.empty
          )
        }
      }

    private def mapMapForTotalTransformers[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        iterator: Expr[Iterator[(FromK, FromV)]],
        factoryEither: Either[Expr[Factory[(ToK, ToV), To]], Expr[Factory[(ToK, ToV), partial.Result[To]]]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): MIO[TransformationExpr[To]] = {
      implicit val TupleFromKVType: Type[(FromK, FromV)] = Type.of[(FromK, FromV)]
      implicit val TupleToKVType: Type[(ToK, ToV)] = Type.of[(ToK, ToV)]
      LambdaBuilder
        .of2[FromK, FromV](FreshName.FromPrefix("key"), FreshName.FromPrefix("value"))
        .traverse[MIO, (Expr[ToK], Expr[ToV])] { case (key, value) =>
          deriveKeyMapping[From, To, FromK, ToK](key)
            .map(_.ensureTotal)
            .parTuple(deriveValueMapping[From, To, FromV, ToV](value).map(_.ensureTotal))
        }
        .flatMap { builder =>
          def iteratorMapTo[ToOrPartialTo: Type](
              factory: Expr[Factory[(ToK, ToV), ToOrPartialTo]]
          ): Expr[ToOrPartialTo] =
            // We're constructing:
            // '{ ${ iterator }.map{ case (key, value) =>
            //    (${ resultToKey }, ${ resultToValue })
            //    }
            // }.to(${ factory }) }
            iteratorToCompat(
              iteratorMapCompat(
                iterator,
                function2TupledCompat(builder.buildWith { case (toKeyResult, toValueResult) =>
                  tuple2ExprCompat(toKeyResult, toValueResult)
                })
              ),
              factory
            )

          factoryEither match {
            case Left(totalFactory)    => totalExpr(iteratorMapTo(totalFactory))
            case Right(partialFactory) => partialExpr(iteratorMapTo(partialFactory))
          }
        }
    }

    private def mapMapForPartialTransformers[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        iterator: Expr[Iterator[(FromK, FromV)]],
        factoryEither: Either[Expr[Factory[(ToK, ToV), To]], Expr[Factory[(ToK, ToV), partial.Result[To]]]],
        failFast: Expr[Boolean],
        isConversionFromMap: Boolean // or from any sequence of tuples
    )(implicit
        ctx: TransformationContext[From, To]
    ): MIO[TransformationExpr[To]] = {
      implicit val AnyT: Type[Any] = AnyType
      implicit val TupleFromKVType: Type[(FromK, FromV)] = Type.of[(FromK, FromV)]
      implicit val TupleToKVType: Type[(ToK, ToV)] = Type.of[(ToK, ToV)]
      implicit val IntType: Type[Int] = Type.of[Int]
      implicit val IndexedPairType: Type[((FromK, FromV), Int)] = Type.of[((FromK, FromV), Int)]
      if (isConversionFromMap) {
        // We're constructing:
        // '{ partial.Result.traverse[To, ($FromK, $FromV), ($ToK, $ToV)](
        //   ${ iterator },
        //   { case (key, value) =>
        //     val _ = key
        //     val _ = value
        //     partial.Result.product(
        //       ${ resultToKey }.unsealErrorPath.prependErrorPath(partial.PathElement.MapKey(key)),
        //       ${ resultToValue }.unsealErrorPath.prependErrorPath(partial.PathElement.MapValue(key),
        //       ${ failFast }
        //     )
        //   },
        //   ${ failFast }
        // )(${ factory })
        LambdaBuilder
          .of2[FromK, FromV](FreshName.FromPrefix("key"), FreshName.FromPrefix("value"))
          .traverse[
            MIO,
            ((Expr[partial.Result[ToK]], Expr[FromK]), (Expr[partial.Result[ToV]], Expr[FromV]))
          ] { case (key, value) =>
            deriveKeyMapping[From, To, FromK, ToK](key)
              .map(_.ensurePartial -> key)
              .parTuple(deriveValueMapping[From, To, FromV, ToV](value).map(_.ensurePartial -> value))
          }
          .flatMap { builder =>
            val lambda = function2TupledCompat[FromK, FromV, partial.Result[(ToK, ToV)]](builder.buildWith {
              case ((keyResult, key), (valueResult, value)) =>
                Expr.quote {
                  Expr.splice(Expr.suppressUnused(key))
                  Expr.splice(Expr.suppressUnused(value))
                  Expr.splice {
                    ChimneyExpr.PartialResult.product(
                      keyResult.unsealErrorPath.prependErrorPath(
                        ChimneyExpr.PathElement.MapKey(key.upcast[Any]).upcast[partial.PathElement]
                      ),
                      valueResult.unsealErrorPath.prependErrorPath(
                        ChimneyExpr.PathElement.MapValue(key.upcast[Any]).upcast[partial.PathElement]
                      ),
                      failFast
                    )
                  }
                }
            })

            def partialResultTraverse[ToOrPartialTo: Type](
                factory: Expr[Factory[(ToK, ToV), ToOrPartialTo]]
            ): Expr[partial.Result[ToOrPartialTo]] =
              ChimneyExpr.PartialResult.traverse[ToOrPartialTo, (FromK, FromV), (ToK, ToV)](
                iterator,
                lambda,
                failFast,
                factory
              )

            factoryEither match {
              case Left(totalFactory)   => partialExpr(partialResultTraverse(totalFactory))
              case Right(partialResult) => partialExpr(partialResultTraverse(partialResult).flatten)
            }
          }
      } else {
        // We're constructing:
        // '{ partial.Result.traverse[To, (($FromK, $FromV), Int), ($ToK, $ToV)](
        //   ${ iterator }.zipWithIndex,
        //   { case (pair, idx) =>
        //     partial.Result.product(
        //       {
        //         val key = pair._1
        //         ${ resultToKey }
        //           .unsealErrorPath
        //           .prependErrorPath(partial.PathElement.Accessor("_1"))}
        //           .prependErrorPath(partial.PathElement.Index(idx))
        //       },
        //       {
        //         val value = pair._2
        //         ${ resultToValue }
        //           .unsealErrorPath
        //           .prependErrorPath(partial.PathElement.Accessor("_2"))}
        //           .prependErrorPath(partial.PathElement.Index(idx))
        //       },
        //       ${ failFast }
        //     )
        //   },
        //   ${ failFast }
        // )(${ factory })
        LambdaBuilder
          .of2[(FromK, FromV), Int](FreshName.FromPrefix("pair"), FreshName.FromPrefix("idx"))
          .traverse[MIO, Expr[partial.Result[(ToK, ToV)]]] { case (pairExpr, indexExpr) =>
            val pairGetters = ProductType.parseExtraction[(FromK, FromV)].get.extraction
            val _1 = pairGetters("_1")
            val _2 = pairGetters("_2")
            import _1.{Underlying as From_1, value as getter_1}, _2.{Underlying as From_2, value as getter_2}
            val keyResultVal = ValDefs
              .createVal(getter_1.get(pairExpr).upcast[FromK], FreshName.FromPrefix("key"))
              .traverse[MIO, (Expr[partial.Result[ToK]], Expr[FromK])] { key =>
                deriveKeyMapping[From, To, FromK, ToK](key).map(_.ensurePartial -> key)
              }
            val valueResultVal = ValDefs
              .createVal(getter_2.get(pairExpr).upcast[FromV], FreshName.FromPrefix("value"))
              .traverse[MIO, (Expr[partial.Result[ToV]], Expr[FromV])] { value =>
                deriveValueMapping[From, To, FromV, ToV](value).map(_.ensurePartial -> value)
              }
            keyResultVal.parTuple(valueResultVal).map { case (keyVD, valueVD) =>
              ChimneyExpr.PartialResult.product(
                keyVD
                  .use { case (keyResult, key) =>
                    Expr.quote {
                      Expr.splice(Expr.suppressUnused(key))
                      Expr.splice(keyResult)
                    }
                  }
                  .unsealErrorPath
                  .prependErrorPath(
                    ChimneyExpr.PathElement.Accessor(Expr("_1")).upcast[partial.PathElement]
                  )
                  .prependErrorPath(
                    ChimneyExpr.PathElement.Index(indexExpr).upcast[partial.PathElement]
                  ),
                valueVD
                  .use { case (valueResult, value) =>
                    Expr.quote {
                      Expr.splice(Expr.suppressUnused(value))
                      Expr.splice(valueResult)
                    }
                  }
                  .unsealErrorPath
                  .prependErrorPath(
                    ChimneyExpr.PathElement.Accessor(Expr("_2")).upcast[partial.PathElement]
                  )
                  .prependErrorPath(
                    ChimneyExpr.PathElement.Index(indexExpr).upcast[partial.PathElement]
                  ),
                failFast
              )
            }
          }
          .flatMap { builder =>
            val lambda = function2TupledCompat(builder.build[partial.Result[(ToK, ToV)]])

            def partialResultTraverse[ToOrPartialTo: Type](
                factory: Expr[Factory[(ToK, ToV), ToOrPartialTo]]
            ): Expr[partial.Result[ToOrPartialTo]] =
              ChimneyExpr.PartialResult.traverse[ToOrPartialTo, ((FromK, FromV), Int), (ToK, ToV)](
                iteratorZipWithIndexCompat(iterator),
                lambda,
                failFast,
                factory
              )

            factoryEither match {
              case Left(totalFactory)   => partialExpr(partialResultTraverse(totalFactory))
              case Right(partialResult) => partialExpr(partialResultTraverse(partialResult).flatten)
            }
          }
      }
    }

    private trait TotallyOrPartiallyBuildMap[Collection] {

      type Key
      implicit val Key: Type[Key]

      type Value
      implicit val Value: Type[Value]

      def factory
          : Either[Expr[Factory[(Key, Value), Collection]], Expr[Factory[(Key, Value), partial.Result[Collection]]]]

      def iterator(collection: Expr[Collection]): Expr[Iterator[(Key, Value)]]

      def to[Collection2: Type](
          collection: Expr[Collection],
          factory: Expr[Factory[(Key, Value), Collection2]]
      ): Expr[Collection2]
    }
    private object TotallyOrPartiallyBuildMap {

      def parse[M](implicit M: Type[M]): Option[TotallyOrPartiallyBuildMap[M]] =
        TotallyOrPartiallyBuildIterable.parse[M].flatMap(it => it.value.asMap.map(it -> _)).map {
          case (it, (key, value)) =>
            import it.Underlying as Inner, key.Underlying as Key0, value.Underlying as Value0
            implicit val TupleKey0Value0Type: Type[(Key0, Value0)] = tuple2TypeCompat[Key0, Value0]
            new TotallyOrPartiallyBuildMap[M] {

              type Key = Key0
              val Key: Type[Key] = Key0

              type Value = Value0
              val Value: Type[Value] = Value0

              def factory: Either[Expr[Factory[(Key, Value), M]], Expr[Factory[(Key, Value), partial.Result[M]]]] = {
                implicit val FactoryInnerMType: Type[Factory[Inner, M]] = factoryTypeCompat[Inner, M]
                implicit val FactoryInnerPartialMType: Type[Factory[Inner, partial.Result[M]]] =
                  factoryTypeCompat[Inner, partial.Result[M]]
                implicit val FactoryKVMType: Type[Factory[(Key0, Value0), M]] = factoryTypeCompat[(Key0, Value0), M]
                implicit val FactoryKVPartialMType: Type[Factory[(Key0, Value0), partial.Result[M]]] =
                  factoryTypeCompat[(Key0, Value0), partial.Result[M]]
                it.value.factory match {
                  case Left(totalFactory) =>
                    Left(totalFactory.upcast[Factory[(Key, Value), M]])
                  case Right(partialFactory) =>
                    Right(partialFactory.upcast[Factory[(Key, Value), partial.Result[M]]])
                }
              }

              def iterator(collection: Expr[M]): Expr[Iterator[(Key, Value)]] = {
                implicit val IteratorInnerType: Type[Iterator[Inner]] = iteratorTypeCompat[Inner]
                implicit val IteratorKVType: Type[Iterator[(Key0, Value0)]] = iteratorTypeCompat[(Key0, Value0)]
                it.value.iterator(collection).upcast[Iterator[(Key0, Value0)]]
              }

              def to[Collection2: Type](
                  collection: Expr[M],
                  factory: Expr[Factory[(Key, Value), Collection2]]
              ): Expr[Collection2] = {
                implicit val FactoryKVC2Type: Type[Factory[(Key0, Value0), Collection2]] =
                  factoryTypeCompat[(Key0, Value0), Collection2]
                implicit val FactoryInnerC2Type: Type[Factory[Inner, Collection2]] =
                  factoryTypeCompat[Inner, Collection2]
                it.value.to(collection, factory.upcast[Factory[Inner, Collection2]])
              }
            }
        }
      final def unapply[M](M: Type[M]): Option[TotallyOrPartiallyBuildMap[M]] = parse(using M)
    }
  }
}
