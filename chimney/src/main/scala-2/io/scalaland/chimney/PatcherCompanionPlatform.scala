package io.scalaland.chimney

import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros

import scala.language.experimental.macros

private[chimney] trait PatcherCompanionPlatform { this: Patcher.type =>

  /** Provides implicit [[io.scalaland.chimney.Patcher]] instance
   * for arbitrary types.
   *
   * @tparam T     type of object to apply patch to
   * @tparam Patch type of patch object
   * @return [[io.scalaland.chimney.Patcher]] type class instance
   * @since 0.2.0
   */
  implicit def derive[T, Patch]: Patcher[T, Patch] =
    macro PatcherMacros.derivePatcherImpl[T, Patch]
}
