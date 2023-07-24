package io.scalaland.chimney

import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros

private[chimney] trait PatcherCompanionPlatform { this: Patcher.type =>

  /** Provides implicit [[io.scalaland.chimney.Patcher]] instance
    * for arbitrary types.
    *
    * @tparam A     type of object to apply patch to
    * @tparam Patch type of patch object
    * @return [[io.scalaland.chimney.Patcher]] type class instance
    * @since 0.8.0
    */
  implicit inline def derive[A, Patch]: Patcher[A, Patch] = ${ PatcherMacros.derivePatcher[A, Patch] }
}
