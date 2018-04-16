package io.scalaland.chimney

import io.scalaland.chimney.internal.ChimneyMacros
import scala.language.experimental.macros

object dsl2 {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    final def into[To]: TransformerInto[From, To] =
      new TransformerInto(source)

    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }


  final class TransformerInto[From, To](val source: From) {

    def disableDefaultValues: TransformerInto[From, To] =
      macro ChimneyMacros.disableDefaultValuesImpl[From, To]

    def withFieldConst[T](selector: To => T, value: T): TransformerInto[From, To] =
      macro ChimneyMacros.withFieldConstImpl[From, To, T]

    def withFieldComputed[T](selector: To => T, map: From => T): TransformerInto[From, To] =
      macro ChimneyMacros.withFieldComputedImpl[From, To, T]

    def transform: To =
      macro ChimneyMacros.transformImpl[From, To]
  }
}

