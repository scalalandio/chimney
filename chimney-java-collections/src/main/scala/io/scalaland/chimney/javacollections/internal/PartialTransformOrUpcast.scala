package io.scalaland.chimney.javacollections.internal

import io.scalaland.chimney.{partial, PartialTransformer}

/** Since [[io.scalaland.chimney.PartialTransformer]] is NOT automatically provided for identity/subtype transformation,
  * and we want to allow such things without enabling whole recursive auto-derivation we use this trick.
  */
trait PartialTransformOrUpcast[From, To] {

  def transform(src: From, failFast: Boolean): partial.Result[To]
}
object PartialTransformOrUpcast extends PartialTransformOrUpcastLowPriority {

  implicit def userProvidedTransformerExists[From, To](implicit
      transformer: PartialTransformer[From, To]
  ): PartialTransformOrUpcast[From, To] =
    (from, failFast) => transformer.transform(from, failFast)
}
private[internal] trait PartialTransformOrUpcastLowPriority {

  implicit def sourceIsSubtypeOfTarget[From, To](implicit ev: From <:< To): PartialTransformOrUpcast[From, To] =
    (from, _) => partial.Result.fromValue(ev(from))
}
