package io.scalaland.chimney.integrations

import io.scalaland.chimney.partial

import scala.collection.compat.*

trait PartiallyBuildIterable[Collection] {
  type Item

  def partialFactory: Factory[Item, partial.Result[Collection]]

  def iterable[A](collection: Collection): Iterable[A]

  def to[Collection2](collection: Collection, factory: Factory[Item, Collection2]): Collection2 =
    iterable(collection).to(factory)
}
object PartiallyBuildIterable {
  type Of[Collection, Item0] = PartiallyBuildIterable[Collection] { type Item = Item0 }
}
