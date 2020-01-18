package io.scalaland.chimney

import io.scalaland.chimney.internal.PatcherCfg
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.dsl.{PatcherInto, TransformerDefinition, TransformerInto}

import scala.language.experimental.macros

object dsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    final def into[To]: TransformerInto[From, To, TransformerCfg.Empty] =
      new TransformerInto(source, new TransformerDefinition[From, To, TransformerCfg.Empty](Map.empty, Map.empty))

    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }

  implicit class PatcherOps[T](val obj: T) extends AnyVal {

    final def using[P](patch: P): PatcherInto[T, P, PatcherCfg.Empty] =
      new PatcherInto[T, P, PatcherCfg.Empty](obj, patch)

    final def patchWith[P](patch: P)(implicit patcher: Patcher[T, P]): T =
      patcher.patch(obj, patch)
  }
}
