package io.scalaland.chimney.integrations

import io.scalaland.chimney.partial

import scala.collection.compat.*

/** Tells Chimney how to interact with `Collection` type to align its behavior with e.g. NonEmpty* collections of
  * `Item`s.
  *
  * It's factory should perform validation of a collection once all its elements has been obtained.
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#custom-collection-types]] for more details
  *
  * @tparam Collection
  *   type storing collection of `Item`s - has to be proper type, not higher-kinded type
  * @tparam Item
  *   type of internal items
  *
  * @since 1.0.0
  */
trait PartiallyBuildIterable[Collection, Item] {

  /** Factory of the `Collection`, validated with [[partial.Result]]. */
  def partialFactory: Factory[Item, partial.Result[Collection]]

  /** Creates [[Iterator]] for the `Collection`. */
  def iterator(collection: Collection): Iterator[Item]

  /** Converts Collection into `Collection2`. */
  def to[Collection2](collection: Collection, factory: Factory[Item, Collection2]): Collection2 =
    FactoryCompat.iteratorTo(iterator(collection), factory)

  /** Useful since this class is invariant. */
  def widen[Collection2 >: Collection]: PartiallyBuildIterable[Collection2, Item] =
    this.asInstanceOf[PartiallyBuildIterable[Collection2, Item]]
}
