package io.scalaland.chimney.integrations

import scala.collection.compat.*

trait TotallyBuildIterable[Collection, Item] {

  def totalFactory: Factory[Item, Collection]

  def iterator(collection: Collection): Iterator[Item]

  def to[Collection2](collection: Collection, factory: Factory[Item, Collection2]): Collection2 =
    iterator(collection).to(factory)
}
