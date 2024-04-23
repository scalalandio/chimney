package io.scalaland.chimney.integrations

/** Subtype of [[scala.collection.compat.Factory]].
  *
  * The reason this trait exist is to make it easier to cross-compile Factories - scala.collection.compat contains only
  * an alias, so it can be used to summon existing factories but not for creating new ones.
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
}
