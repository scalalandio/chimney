package io.scalaland.chimney

import io.scalaland.chimney.internal.PatcherCfg
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.dsl.{PatcherUsing, TransformerDefinition, TransformerInto}

import scala.language.experimental.macros

object dsl {

  implicit class TransformerOps[From](private val source: From) extends AnyVal {

    final def into[To]: TransformerInto[From, To, TransformerCfg.Empty] =
      new TransformerInto(source, new TransformerDefinition[From, To, TransformerCfg.Empty](Map.empty, Map.empty))

    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }

  implicit class PatcherOps[T](private val obj: T) extends AnyVal {

    final def using[P](patch: P): PatcherUsing[T, P, PatcherCfg.Empty] =
      new PatcherUsing[T, P, PatcherCfg.Empty](obj, patch)

    final def patchUsing[P](patch: P)(implicit patcher: Patcher[T, P]): T =
      patcher.patch(obj, patch)

    @deprecated("please use .patchUsing", "0.4.0")
    final def patchWith[P](patch: P)(implicit patcher: Patcher[T, P]): T =
      obj.patchUsing(patch)
  }
}
