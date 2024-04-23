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
trait FactoryCompat[-A, +CC] extends scala.collection.generic.CanBuildFrom[Any, A, CC] {

  def newBuilder: scala.collection.mutable.Builder[A, CC]

  override def apply(from: Any): scala.collection.mutable.Builder[A, CC] = newBuilder
  override def apply(): scala.collection.mutable.Builder[A, CC] = newBuilder
}
object FactoryCompat {

  trait Builder[-A, +CC] extends scala.collection.mutable.Builder[A, CC] {

    def addOne(elem: A): this.type

    override def +=(elem: A): this.type = addOne(elem)
  }

  // Scala 2.12 expects Factory[Item, Coll[Item]] and we have to use silly tricks to put arbitrary Collection there

  def arrayTo[Item, Collection](
      array: Array[Item],
      factory: scala.collection.compat.Factory[Item, Collection]
  ): Collection = {
    def scala2_12workaround[Coll[_]](
        factoryButDifferentType: scala.collection.compat.Factory[Item, Coll[Item]]
    ): Coll[Item] = array.to(factoryButDifferentType)
    type Coll2[A] = Collection
    scala2_12workaround[Coll2](factory)
  }

  def iterableTo[Item, Collection](
      iterable: Iterable[Item],
      factory: scala.collection.compat.Factory[Item, Collection]
  ): Collection = {
    def scala2_12workaround[Coll[_]](
        factoryButDifferentType: scala.collection.compat.Factory[Item, Coll[Item]]
    ): Coll[Item] = iterable.to(factoryButDifferentType)
    type Coll2[A] = Collection
    scala2_12workaround[Coll2](factory)
  }

  def iteratorTo[Item, Collection](
      iterator: Iterator[Item],
      factory: scala.collection.compat.Factory[Item, Collection]
  ): Collection = {
    def scala2_12workaround[Coll[_]](
        factoryButDifferentType: scala.collection.compat.Factory[Item, Coll[Item]]
    ): Coll[Item] = iterator.to(factoryButDifferentType)
    type Coll2[A] = Collection
    scala2_12workaround[Coll2](factory)
  }
}
