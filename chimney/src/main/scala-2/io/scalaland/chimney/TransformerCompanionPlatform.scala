package io.scalaland.chimney

//import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros
import io.scalaland.chimney.internal.compiletime.derivation.transformer.NewTransformerMacros

import scala.language.experimental.macros

private[chimney] trait TransformerCompanionPlatform { this: Transformer.type =>

  /** Provides [[io.scalaland.chimney.Transformer]] derived with the default settings.
   *
   * When transformation can't be derived, it results with compilation error.
   *
   * @tparam From type of input value
   * @tparam To   type of output value
   * @return [[io.scalaland.chimney.Transformer]] type class instance
   * @since 0.2.0
   */
  implicit def derive[From, To]: Transformer[From, To] =
    macro NewTransformerMacros.deriveTotalTransformerImpl[From, To]
//    macro TransformerBlackboxMacros.deriveTransformerImpl[From, To]
}
