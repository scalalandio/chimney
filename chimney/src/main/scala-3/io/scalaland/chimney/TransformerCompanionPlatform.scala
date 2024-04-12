package io.scalaland.chimney

import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros

private[chimney] trait TransformerCompanionPlatform { this: Transformer.type =>

  /** Provides [[io.scalaland.chimney.Transformer]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @return
    *   [[io.scalaland.chimney.Transformer]] type class instance
    *
    * @since 0.8.0
    */
  inline def derive[From, To]: Transformer[From, To] =
    ${ TransformerMacros.deriveTotalTransformerWithDefaults[From, To] }
}

private[chimney] trait TransformerAutoDerivedCompanionPlatform { this: Transformer.AutoDerived.type =>

  /** Provides [[io.scalaland.chimney.Transformer]] derived with the default settings.
    *
    * This instance WILL NOT be visible for recursive derivation (automatic, semiautomatic, inlined), which is how it
    * differs from [[io.scalaland.chimney.auto#deriveAutomaticTransformer]].
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @return
    *   [[io.scalaland.chimney.Transformer.AutoDerived]] type class instance
    *
    * @since 0.8.0
    */
  implicit inline def deriveAutomatic[From, To]: Transformer.AutoDerived[From, To] =
    ${ TransformerMacros.deriveTotalTransformerWithDefaults[From, To] }
}
