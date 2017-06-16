package io.scalaland.chimney

import io.scalaland.chimney.internal.TransformerInto
import shapeless.HNil

/** Provides syntax for API user. */
object dsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    final def into[To]: TransformerInto[From, To, HNil] =
      new TransformerInto(source, HNil)

    final def transformInto[To](implicit derivedTransformer: DerivedTransformer[From, To, HNil]): To =
      derivedTransformer.transform(source, HNil)
  }

  implicit class PatcherOps[T](val obj: T) extends AnyVal {

    final def patchWith[P](patch: P)(implicit patcher: Patcher[T, P]): T =
      patcher.patch(obj, patch)
  }
}
