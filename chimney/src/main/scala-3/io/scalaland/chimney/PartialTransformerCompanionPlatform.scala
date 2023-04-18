package io.scalaland.chimney

private[chimney] trait PartialTransformerCompanionPlatform { this: PartialTransformer.type =>

  /** Provides [[io.scalaland.chimney.PartialTransformer]] derived with the default settings.
   *
   * When transformation can't be derived, it results with compilation error.
   *
   * @tparam From type of input value
   * @tparam To   type of output value
   * @return [[io.scalaland.chimney.PartialTransformer]] type class definition
   * @since 0.8.0
   */
  implicit inline def derive[From, To]: PartialTransformer[From, To] = ???
}
