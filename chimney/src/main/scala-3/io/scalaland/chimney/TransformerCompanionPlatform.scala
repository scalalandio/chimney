package io.scalaland.chimney

private[chimney] trait TransformerCompanionPlatform { this: Transformer.type =>

  /** Provides [[io.scalaland.chimney.Transformer]] derived with the default settings.
   *
   * When transformation can't be derived, it results with compilation error.
   *
   * @tparam From type of input value
   * @tparam To   type of output value
   * @return [[io.scalaland.chimney.Transformer]] type class instance
   * @since 0.8.0
   */
  implicit inline def derive[From, To]: Transformer[From, To] = ???
}
