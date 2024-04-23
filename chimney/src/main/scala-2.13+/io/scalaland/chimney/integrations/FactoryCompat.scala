package io.scalaland.chimney.integrations

/** Subtype of [[scala.collection.compat.Factory]].
  *
  * The reason this trait exist is to make it easier to cross-compile Factories - scala.collection.compat contains only
  * an alias, so it can be used to summon existing factories but not for creating new ones.
  *
  * @since 1.0.0
  */
trait FactoryCompat[-A, +CC] extends scala.collection.Factory[A, CC] {

  def newBuilder: scala.collection.mutable.Builder[A, CC]

  override def fromSpecific(it: scala.collection.IterableOnce[A]): CC = newBuilder.addAll(it).result()
}
object FactoryCompat {

  trait Builder[-A, +CC] extends scala.collection.mutable.Builder[A, CC]
}
