package io.scalaland.chimney

import io.scalaland.chimney.internal.{Dsl2Macros, TransformerMacros}
import scala.language.experimental.macros


object dsl2 {


  implicit class TransformerOps[From](val source: From) extends AnyVal {

    final def into[To]: TransformerInto[From, To] =
      new TransformerInto(source)

    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }

  case class Config(lookupDefaultValues: Boolean)

  object Config {

    val default = Config(lookupDefaultValues = true)
  }

  final class TransformerInto[From, To](val source: From) {

    def disableDefaultValues: TransformerInto[From, To] =
      macro Dsl2Macros.disableDefaultValuesImpl[From, To]

    def withFieldConst[T](selector: To => T, value: T): TransformerInto[From, To] =
      macro Dsl2Macros.withFieldConstImpl[From, To, T]

    def withFieldComputed[T](selector: To => T, map: From => T): TransformerInto[From, To] =
      macro Dsl2Macros.withFieldComputedImpl[From, To, T]

    def transform: To =
      macro Dsl2Macros.transformImpl[From, To]

  }
}

