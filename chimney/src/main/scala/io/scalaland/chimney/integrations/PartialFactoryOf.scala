package io.scalaland.chimney.integrations

import io.scalaland.chimney.partial
import scala.collection.compat.IterableOnce

trait PartialFactoryOf[A, CC] {

  def newBuilder: PartialFactoryOf.Builder[A, CC]

  def partialFromSpecific(it: IterableOnce[A]): partial.Result[CC]

  final def narrowPartial[CC2 >: CC]: TotalFactoryOf[A, CC2] = this.asInstanceOf[TotalFactoryOf[A, CC2]]
}
object PartialFactoryOf {

  trait Builder[A, CC] {
    def addOne(a: A): Unit

    def partialResult(): partial.Result[CC]
  }

  // TODO: implicit to Factory
}