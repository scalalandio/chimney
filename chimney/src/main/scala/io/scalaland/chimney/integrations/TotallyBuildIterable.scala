package io.scalaland.chimney.integrations

import scala.collection.compat.*

/** Tells Chimney how to interact with `Collection` type to align its behavior with [[Seq]] of `Item`s.
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
trait TotallyBuildIterable[Collection, Item] {

  /** Factory of the `Collection` */
  def totalFactory: Factory[Item, Collection]

  /** Creates [[Iterator]] for the `Collection`. */
  def iterator(collection: Collection): Iterator[Item]

  /** Converts Collection into `Collection2`. */
  def to[Collection2](collection: Collection, factory: Factory[Item, Collection2]): Collection2 =
    FactoryCompat.iteratorTo(iterator(collection), factory)
}
