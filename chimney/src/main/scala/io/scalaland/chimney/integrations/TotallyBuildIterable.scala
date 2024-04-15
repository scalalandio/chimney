package io.scalaland.chimney.integrations

import scala.collection.compat.*

trait TotallyBuildIterable[Collection, Item] {

  def totalFactory: Factory[Item, Collection]

  def iterable(collection: Collection): Iterable[Item]

  def to[Collection2](collection: Collection, factory: Factory[Item, Collection2]): Collection2 =
    iterable(collection).to(factory)
}
