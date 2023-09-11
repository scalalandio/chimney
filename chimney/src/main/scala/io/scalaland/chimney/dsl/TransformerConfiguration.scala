package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.TransformerFlags

/** Type-level set of derivation flags that can be shared between derivations through implicit scope.
  *
  * @tparam Flags type-level encoded flags
  *
  * @since 0.6.0
  */
final class TransformerConfiguration[Flags <: TransformerFlags]
    extends TransformerFlagsDsl[TransformerConfiguration.UpdateFlag, Flags]

/** @since 0.6.0 */
object TransformerConfiguration {

  type UpdateFlag[F1 <: TransformerFlags] = TransformerConfiguration[F1]

  /** @since 0.6.0 */
  implicit val default: TransformerConfiguration[TransformerFlags.Default] =
    new TransformerConfiguration[TransformerFlags.Default]
}
