package io.scalaland.chimney

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros

/** Imports only automatic derivation implicits
  *
  * @since 0.8.0
  */
object auto extends LowPriorityAutoInstances

private[chimney] trait LowPriorityAutoInstances { this: auto.type =>

  /** Provides [[io.scalaland.chimney.Transformer]] derived with the default settings.
    *
    * This instance WILL be visible for recursive derivation (automatic, semiautomatic, inlined),
    * which is how it differs from [[io.scalaland.chimney.Transformer.AutoDerived#deriveAutomatic]].
    *
    * @tparam From type of input value
    * @tparam To   type of output value
    * @return [[io.scalaland.chimney.Transformer]] type class instance
    *
    * @since 0.8.0
    */
  implicit inline def deriveAutomaticTransformer[From, To]: Transformer[From, To] =
    ${ TransformerMacros.deriveTotalTransformerWithDefaults[From, To] }

  /** Provides [[io.scalaland.chimney.PartialTransformer]] derived with the default settings.
    *
    * This instance WILL be visible for recursive derivation (automatic, semiautomatic, inlined),
    * which is how it differs from [[io.scalaland.chimney.PartialTransformer.AutoDerived#deriveAutomatic]].
    *
    * @tparam From type of input value
    * @tparam To   type of output value
    * @return [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.8.0
    */
  implicit inline def deriveAutomaticPartialTransformer[From, To]: PartialTransformer[From, To] =
    ${ TransformerMacros.derivePartialTransformerWithDefaults[From, To] }

  /** Provides [[io.scalaland.chimney.Patcher]] derived with the default settings.
    *
    * This instance WILL be visible for recursive derivation (automatic, semiautomatic, inlined),
    * which is how it differs from [[io.scalaland.chimney.Patcher.AutoDerived#deriveAutomatic]].
    *
    * @tparam A     type of object to apply patch to
    * @tparam Patch type of patch object
    * @return [[io.scalaland.chimney.Patcher]] type class instance
    *
    * @since 0.8.0
    */
  implicit inline def deriveAutomaticPatcher[From, To]: Patcher[From, To] =
    ${ PatcherMacros.derivePatcherWithDefaults[From, To] }
}
