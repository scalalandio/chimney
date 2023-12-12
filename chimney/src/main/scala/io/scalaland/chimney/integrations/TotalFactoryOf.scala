package io.scalaland.chimney.integrations

import io.scalaland.chimney.partial
import scala.collection.compat.IterableOnce

// TODO: add deprecated type alias in io.scalaland.chimney.javacollections
trait TotalFactoryOf[A, CC] extends PartialFactoryOf[A, CC] {

  def newBuilder: TotalFactoryOf.Builder[A, CC]

  def totalFromSpecific(it: IterableOnce[A]): CC

  final def narrowTotal[CC2 >: CC]: TotalFactoryOf[A, CC2] = this.asInstanceOf[TotalFactoryOf[A, CC2]]

  final def partialFromSpecific(it: IterableOnce[A]): partial.Result[CC] =
    partial.Result.fromCatching(totalFromSpecific(it))
}
object TotalFactoryOf {

  trait Builder[A, CC] extends PartialFactoryOf.Builder[A, CC] {
    def addOne(a: A): Unit

    def result(): CC

    final def partialResult(): partial.Result[CC] = partial.Result.fromCatching(result())
  }

  // TODO: implicit to Factory
}
