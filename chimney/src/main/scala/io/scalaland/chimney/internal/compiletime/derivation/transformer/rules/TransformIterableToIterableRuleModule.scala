package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

private[compiletime] trait TransformIterableToIterableRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformIterableToIterableRule extends Rule("IterableToIterable") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (IterableOrArray(from2), IterableOrArray(to2)) =>
          Existential.use2(from2, to2) {
            implicit From2: Type[from2.Underlying] => implicit To2: Type[to2.Underlying] =>
              (fromIorA: IterableOrArray[From, from2.Underlying], toIorA: IterableOrArray[To, to2.Underlying]) =>
                ExprPromise
                  .promise[from2.Underlying](ExprPromise.NameGenerationStrategy.FromExpr(ctx.src))
                  .traverse { (newFromSrc: Expr[from2.Underlying]) =>
                    deriveRecursiveTransformationExpr[from2.Underlying, to2.Underlying](newFromSrc)
                  }
                  .flatMap { (to2P: ExprPromise[from2.Underlying, TransformationExpr[to2.Underlying]]) =>
                    to2P.foldTransformationExpr { (totalP: ExprPromise[from2.Underlying, Expr[to2.Underlying]]) =>
                      // After mapping we don't know the exact static type here, but we might check the values
                      // to see if we could skip .to(factory) part
                      lazy val mappedFrom = fromIorA.map(ctx.src)(totalP.fulfilAsLambda[to2.Underlying])
                      if (Type[from2.Underlying] =:= Type[to2.Underlying]) {
                        toIorA.factory.flatMap { (factory: Expr[Factory[to2.Underlying, To]]) =>
                          // We're constructing:
                          // '{ ${ src }.to(Factory[$To, $to2]) }
                          DerivationResult.expandedTotal(
                            fromIorA.to[To](ctx.src)(factory.upcastExpr[Factory[from2.Underlying, To]])
                          )
                        }
                      } else if (mappedFrom.Underlying =:= Type[To]) {
                        // We're constructing:
                        // '{ ${ src }.map(from2 => ${ derivedTo2 }) }
                        DerivationResult.expandedTotal {
                          ExistentialExpr.use(mappedFrom) { implicit Out: Type[mappedFrom.Underlying] => expr =>
                            expr.upcastExpr[To]
                          }
                        }
                      } else {
                        // We're constructing
                        // '{ ${ src }.iterator.map(from2 => ${ derivedTo2 }).to(Factory[$To, $to2]) }
                        toIorA.factory.flatMap { (factory: Expr[Factory[to2.Underlying, To]]) =>
                          DerivationResult.expandedTotal(
                            fromIorA.iterator(ctx.src).map(totalP.fulfilAsLambda[to2.Underlying]).to[To](factory)
                          )
                        }
                      }
                    } { (partialP: ExprPromise[from2.Underlying, Expr[partial.Result[to2.Underlying]]]) =>
                      ctx match {
                        case TransformationContext.ForPartial(src, failFast) =>
                          // We're constructing:
                          // '{ partial.Result.traverse[To, ($from2, Int), $to2](
                          //   ${ src }.iterator.zipWithIndex,
                          //   { case (value, index) =>
                          //     ${ resultTo }.prependErrorPath(partial.PathElement.Index(index))
                          //   },
                          //   ${ failFast }
                          // )(${ factory })
                          toIorA.factory.flatMap { (factory: Expr[Factory[to2.Underlying, To]]) =>
                            DerivationResult.expandedPartial(
                              ChimneyExpr.PartialResult.traverse[To, (from2.Underlying, Int), to2.Underlying](
                                fromIorA.iterator(src).zipWithIndex,
                                partialP
                                  .fulfilAsLambda2(
                                    ExprPromise.promise[Int](ExprPromise.NameGenerationStrategy.FromPrefix("idx"))
                                  ) { (result: Expr[partial.Result[to2.Underlying]], idx: Expr[Int]) =>
                                    result.prependErrorPath(
                                      ChimneyExpr.PathElement.Index(idx).upcastExpr[partial.PathElement]
                                    )
                                  }
                                  .tupled,
                                failFast,
                                factory
                              )
                            )
                          }
                        case TransformationContext.ForTotal(_) =>
                          // TODO: better error
                          DerivationResult.assertionError("Derived Partial Expr for Total Context")
                      }
                    }
                  }
          }
        case _ =>
          DerivationResult.attemptNextRule
      }

    /** Something allowing us to dispatch same-looking-source-code-but-different ASTs for Iterables and Arrays */
    abstract private class IterableOrArray[M, A] {
      def map[B: Type](m: Expr[M])(f: Expr[A => B]): ExistentialExpr

      def to[C: Type](m: Expr[M])(factory: Expr[Factory[A, C]]): Expr[C]

      def iterator(m: Expr[M]): Expr[Iterator[A]]

      def factory: DerivationResult[Expr[Factory[A, M]]]
    }
    private object IterableOrArray {

      def unapply[M](implicit tpe: Type[M]): Option[Existential[IterableOrArray[M, *]]] = tpe match {
        case Type.Map(k, v) =>
          val a = ExistentialType.use2(k, v) { implicit K: Type[k.Underlying] => implicit V: Type[v.Underlying] =>
            ExistentialType[(k.Underlying, v.Underlying)]
          }
          ExistentialType.use(a) { implicit Inner: Type[a.Underlying] =>
            Some(
              Existential[IterableOrArray[M, *], a.Underlying](
                new IterableOrArray[M, a.Underlying] {

                  def iterator(m: Expr[M]): Expr[Iterator[a.Underlying]] =
                    m.widenExpr[Iterable[a.Underlying]].iterator

                  def map[B: Type](m: Expr[M])(f: Expr[a.Underlying => B]): ExistentialExpr =
                    ExistentialExpr.withoutType(m.widenExpr[Iterable[a.Underlying]].map(f))

                  def to[C: Type](m: Expr[M])(factory: Expr[Factory[a.Underlying, C]]): Expr[C] =
                    m.widenExpr[Iterable[a.Underlying]].to(factory)

                  def factory: DerivationResult[Expr[Factory[a.Underlying, M]]] =
                    DerivationResult.summonImplicit[Factory[a.Underlying, M]]
                }
              )
            )
          }
        case Type.Iterable(a) =>
          ExistentialType.use(a) { implicit Inner: Type[a.Underlying] =>
            Some(
              Existential[IterableOrArray[M, *], a.Underlying](
                new IterableOrArray[M, a.Underlying] {

                  def iterator(m: Expr[M]): Expr[Iterator[a.Underlying]] =
                    m.widenExpr[Iterable[a.Underlying]].iterator

                  def map[B: Type](m: Expr[M])(f: Expr[a.Underlying => B]): ExistentialExpr =
                    ExistentialExpr.withoutType(m.widenExpr[Iterable[a.Underlying]].map(f))

                  def to[C: Type](m: Expr[M])(factory: Expr[Factory[a.Underlying, C]]): Expr[C] =
                    m.widenExpr[Iterable[a.Underlying]].to(factory)

                  def factory: DerivationResult[Expr[Factory[a.Underlying, M]]] =
                    DerivationResult.summonImplicit[Factory[a.Underlying, M]]
                }
              )
            )
          }
        case Type.Array(a) =>
          ExistentialType.use(a) { implicit Inner: Type[a.Underlying] =>
            Some(
              Existential[IterableOrArray[M, *], a.Underlying](
                new IterableOrArray[M, a.Underlying] {
                  def iterator(m: Expr[M]): Expr[Iterator[a.Underlying]] =
                    m.widenExpr[Array[a.Underlying]].iterator

                  def map[B: Type](m: Expr[M])(f: Expr[a.Underlying => B]): ExistentialExpr =
                    ExistentialExpr.withoutType(m.widenExpr[Array[a.Underlying]].map(f))
                  def to[C: Type](m: Expr[M])(factory: Expr[Factory[a.Underlying, C]]): Expr[C] =
                    m.widenExpr[Array[a.Underlying]].to(factory)

                  def factory: DerivationResult[Expr[Factory[a.Underlying, M]]] =
                    DerivationResult.summonImplicit[Factory[a.Underlying, M]]

                }
              )
            )
          }
        case _ => None
      }
    }
  }
}
