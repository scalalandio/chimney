package io.scalaland.chimney.integrations

/** Subtype of [[scala.collection.compat.Factory]].
  *
  * The reason this trait exist is to make it easier to cross-compile Factories - scala.collection.compat contains only
  * an alias, so it can be used to summon existing factories but not for creating new ones.
  *
  * This lives in chimney-macro-commons since it is used within macros to handle 2.12 `sth.to(factory)`.
  *
  * @since 1.0.0
  */
trait FactoryCompat[-A, +CC] extends scala.collection.Factory[A, CC] {

  def newBuilder: scala.collection.mutable.Builder[A, CC]

  override def fromSpecific(it: scala.collection.IterableOnce[A]): CC = newBuilder.addAll(it).result()
}
object FactoryCompat extends FactoryCompatScala3Only {

  trait Builder[-A, +CC] extends scala.collection.mutable.Builder[A, CC]

  // Scala 2.12 expects Factory[Item, Coll[Item]] and we have to use silly tricks to put arbitrary Collection there

  def arrayTo[Item, Collection](
      array: Array[Item],
      factory: scala.collection.compat.Factory[Item, Collection]
  ): Collection = array.to(factory)

  def iterableTo[Item, Collection](
      iterable: Iterable[Item],
      factory: scala.collection.compat.Factory[Item, Collection]
  ): Collection = iterable.to(factory)

  def iteratorTo[Item, Collection](
      iterator: Iterator[Item],
      factory: scala.collection.compat.Factory[Item, Collection]
  ): Collection = iterator.to(factory)
}
