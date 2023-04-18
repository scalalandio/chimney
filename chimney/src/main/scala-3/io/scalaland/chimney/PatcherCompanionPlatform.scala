package io.scalaland.chimney

private[chimney] trait PatcherCompanionPlatform { this: Patcher.type =>

  /** Provides implicit [[io.scalaland.chimney.Patcher]] instance
   * for arbitrary types.
   *
   * @tparam T     type of object to apply patch to
   * @tparam Patch type of patch object
   * @return [[io.scalaland.chimney.Patcher]] type class instance
   * @since 0.8.0
   */
  implicit inline def derive[T, Patch]: Patcher[T, Patch] = ???
}
