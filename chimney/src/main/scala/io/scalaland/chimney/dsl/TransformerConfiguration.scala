package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerFlags

/** Type-level set of derivation flags that can be shared between derivations through implicit scope.
  *
  * @since 0.6.0
  */
class TransformerConfiguration[Flags <: TransformerFlags]
    extends FlagsDsl[Lambda[`F1 <: TransformerFlags` => TransformerConfiguration[F1]], Flags]

/** @since 0.6.0 */
object TransformerConfiguration {

  /** @since 0.6.0 */
  implicit val default: TransformerConfiguration[TransformerFlags.Default] =
    new TransformerConfiguration[TransformerFlags.Default]
}
