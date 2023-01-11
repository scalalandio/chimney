package io.scalaland.chimney

import io.scalaland.chimney.internal.macros.dsl.PatcherBlackboxMacros

import scala.language.experimental.macros

/** Type class definition that wraps patching behavior.
  *
  * @tparam T type of object to apply patch to
  * @tparam Patch type of patch object
  *
  * @since 0.1.3
  */
trait Patcher[T, Patch] {

  /** @since 0.1.3 */
  def patch(obj: T, patch: Patch): T
}

/** @since 0.1.3 */
object Patcher {

  /** Provides implicit [[io.scalaland.chimney.Patcher]] instance
    * for arbitrary types.
    *
    * @tparam T type of object to apply patch to
    * @tparam Patch type of patch object
    * @return [[io.scalaland.chimney.Patcher]] type class instance
    *
    * @since 0.2.0
    */
  implicit def derive[T, Patch]: Patcher[T, Patch] =
    macro PatcherBlackboxMacros.derivePatcherImpl[T, Patch]
}
