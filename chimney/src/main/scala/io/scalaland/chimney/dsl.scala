package io.scalaland.chimney

import io.scalaland.chimney.internal.dsl.{TransformerDefinition, TransformerInto}
import io.scalaland.chimney.internal._

import scala.language.experimental.macros

object dsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    final def into[To]: TransformerInto[From, To, Empty] =
      new TransformerInto[From, To, Empty](source, new TransformerDefinition[From, To, Empty](Map.empty, Map.empty))

    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }

  implicit class PatcherOps[T](val obj: T) extends AnyVal {

    final def patchWith[P](patch: P)(implicit patcher: Patcher[T, P]): T =
      patcher.patch(obj, patch)
  }
}
