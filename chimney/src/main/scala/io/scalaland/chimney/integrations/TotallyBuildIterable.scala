package io.scalaland.chimney.integrations

import scala.collection.compat.*

trait TotallyBuildIterable[Collection] {
  type Item

  def totalFactory: Factory[Item, Collection]

  def iterable[A](collection: Collection): Iterable[A]

  def to[Collection2](collection: Collection, factory: Factory[Item, Collection2]): Collection2 =
    iterable(collection).to(factory)
}
object TotallyBuildIterable {
  type Of[Collection, Item0] = TotallyBuildIterable[Collection] { type Item = Item0 }
}
