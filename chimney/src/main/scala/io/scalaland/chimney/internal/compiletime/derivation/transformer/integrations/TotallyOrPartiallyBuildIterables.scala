package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.Factory

trait TotallyOrPartiallyBuildIterables { this: Derivation & hearth.MacroCommons =>

  /** Parent of [[TotallyBuildIterable]] and [[PartiallyBuildIterable]] which gives access to reading values from Expr.
    */
  abstract protected class TotallyOrPartiallyBuildIterable[Collection, Item] {

    def factory: Either[Expr[Factory[Item, Collection]], Expr[Factory[Item, partial.Result[Collection]]]]

    def iterator(collection: Expr[Collection]): Expr[Iterator[Item]]

    /** Splices the per-item body directly into a loop over the collection - Hearth `IsCollection`-backed instances
      * delegate to the provider's `foreach` (e.g. an index-based loop for `Array`s), others walk [[iterator]] with a
      * `while` loop. Leaner than allocating `iterator.map(...)` wrappers wherever a total per-item transformation has
      * to fill a builder.
      */
    def foreach(collection: Expr[Collection])(f: Expr[Item] => Expr[Unit]): Expr[Unit]

    def to[Collection2: Type](
        collection: Expr[Collection],
        factory: Expr[Factory[Item, Collection2]]
    ): Expr[Collection2]

    /** Expression computing the collection's size for pre-allocating a builder (`Builder.sizeHint`), or `None` when no
      * SAFE size expression exists. Safety contract is hearth's `IsCollectionOf.sizeHintForBuilder` (hearth#354): the
      * expression must be cheap and must NOT traverse/consume the collection - single-pass sources stay `None` (the
      * default) - and may evaluate to a negative value at runtime when unknown (callers guard with `>= 0`).
      * Hearth-backed instances delegate to the provider; everything else keeps the default.
      */
    def builderSizeHint(collection: Expr[Collection]): Option[Expr[Int]] = None

    val asMap: Option[(ExistentialType, ExistentialType)]
  }
  object TotallyOrPartiallyBuildIterable {

    def parse[M](implicit M: Type[M]): Option[Existential[TotallyOrPartiallyBuildIterable[M, *]]] =
      TotallyBuildIterable
        .parse[M]
        .asInstanceOf[Option[Existential[TotallyOrPartiallyBuildIterable[M, *]]]]
        .orElse(
          PartiallyBuildIterable.parse[M].asInstanceOf[Option[Existential[TotallyOrPartiallyBuildIterable[M, *]]]]
        )
    final def unapply[M](M: Type[M]): Option[Existential[TotallyOrPartiallyBuildIterable[M, *]]] = parse(using M)
  }

  /** Default [[TotallyOrPartiallyBuildIterable.foreach]] body: a `while` loop over the iterator. */
  @scala.annotation.nowarn("msg=is never used")
  protected def iteratorForeachCompat[A: Type](it: Expr[Iterator[A]])(f: Expr[A] => Expr[Unit]): Expr[Unit] = {
    implicit val IteratorA: Type[Iterator[A]] = Type.of[Iterator[A]]
    Expr.quote {
      val iter = Expr.splice(it)
      while (iter.hasNext) {
        val elem = iter.next()
        Expr.splice(f(Expr.quote(elem)))
      }
    }
  }
}
