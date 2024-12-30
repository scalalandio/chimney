package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.PatcherFlags

/** Type-level set of derivation flags that can be shared between derivations through implicit scope.
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#reusing-the-flags-for-several-transformationspatchings]] for more details
  *
  * @tparam Flags
  *   type-level encoded flags
  *
  * @since 0.8.0
  */
final class PatcherConfiguration[Flags <: PatcherFlags] extends PatcherFlagsDsl[PatcherConfiguration.UpdateFlag, Flags]

/** @since 0.8.0 */
object PatcherConfiguration {

  type UpdateFlag[F1 <: PatcherFlags] = PatcherConfiguration[F1]

  /** @since 0.8.0 */
  implicit val default: PatcherConfiguration[PatcherFlags.Default] =
    new PatcherConfiguration[PatcherFlags.Default]
}
