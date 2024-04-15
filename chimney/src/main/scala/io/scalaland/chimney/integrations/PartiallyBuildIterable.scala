package io.scalaland.chimney.integrations

import io.scalaland.chimney.partial

import scala.collection.compat.*

trait PartiallyBuildIterable[Collection, Item] {

  def partialFactory: Factory[Item, partial.Result[Collection]]

  def iterable(collection: Collection): Iterable[Item]

  def to[Collection2](collection: Collection, factory: Factory[Item, Collection2]): Collection2 =
    iterable(collection).to(factory)
}
