package io.scalaland.chimney.javacollections.internal

import io.scalaland.chimney.Transformer

/** Since [[io.scalaland.chimney.Transformer]] is NOT automatically provided for identity/subtype transformation,
  * and we want to allow such things without enabling whole recursive auto-derivation we use this trick.
  */
trait TransformOrUpcast[From, To] {
  def transform(src: From): To
}
object TransformOrUpcast extends TransformOrUpcastLowPriority {

  implicit def userProvidedTransformerExists[From, To](implicit
      transformer: Transformer[From, To]
  ): TransformOrUpcast[From, To] =
    from => transformer.transform(from)
}
private[internal] trait TransformOrUpcastLowPriority {

  implicit def sourceIsSubtypeOfTarget[From, To](implicit ev: From <:< To): TransformOrUpcast[From, To] =
    from => ev(from)
}
