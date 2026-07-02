package io.scalaland.chimney.integrations

/** Subtype of [[scala.collection.Factory]].
  *
  * The reason this trait exist is to make it easier to cross-compile Factories - it can be used to summon existing
  * factories but also for creating new ones with a single overridable method.
  *
  * (Until the Hearth migration this class lived in - and was published with - chimney-macro-commons; it is a runtime
  * class used by [[TotallyBuildIterable]]/[[PartiallyBuildIterable]] and by user-defined collection integrations, so
  * with the chimney-macro-commons dependency gone it is now defined in chimney itself, keeping the same fully-qualified
  * name.)
  *
  * @since 1.0.0
  */
trait FactoryCompat[-A, +CC] extends scala.collection.Factory[A, CC] {

  def newBuilder: scala.collection.mutable.Builder[A, CC]

  override def fromSpecific(it: scala.collection.IterableOnce[A]): CC = newBuilder.addAll(it).result()
}
object FactoryCompat extends FactoryCompatScala3Only {

  trait Builder[-A, +CC] extends scala.collection.mutable.Builder[A, CC]

  // Historically (Scala 2.12) Factory[Item, Coll[Item]] was expected and silly tricks were needed to put an arbitrary
  // Collection there - these helpers are kept since they are part of the published API.

  def arrayTo[Item, Collection](
      array: Array[Item],
      factory: scala.collection.Factory[Item, Collection]
  ): Collection = array.to(factory)

  def iterableTo[Item, Collection](
      iterable: Iterable[Item],
      factory: scala.collection.Factory[Item, Collection]
  ): Collection = iterable.to(factory)

  def iteratorTo[Item, Collection](
      iterator: Iterator[Item],
      factory: scala.collection.Factory[Item, Collection]
  ): Collection = iterator.to(factory)
}
