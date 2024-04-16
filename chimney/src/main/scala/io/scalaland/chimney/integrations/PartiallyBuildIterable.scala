package io.scalaland.chimney.integrations

import io.scalaland.chimney.partial

import scala.collection.compat.*

trait PartiallyBuildIterable[Collection, Item] {

  def partialFactory: Factory[Item, partial.Result[Collection]]

  def iterator(collection: Collection): Iterator[Item]

  def to[Collection2](collection: Collection, factory: Factory[Item, Collection2]): Collection2 =
    iterator(collection).to(factory)
}
