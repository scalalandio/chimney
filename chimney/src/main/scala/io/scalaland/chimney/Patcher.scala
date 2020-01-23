package io.scalaland.chimney

import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros

/** Type class definition that wraps patching behavior.
 *
 * @tparam T type of object to apply patch to
 * @tparam Patch type of patch object
 */
trait Patcher[T, Patch] {
  def patch(obj: T, patch: Patch): T
}

object Patcher {

  /** Provides implicit [[io.scalaland.chimney.Patcher]] instance
   * for arbitrary types.
   *
   * @tparam T type of object to apply patch to
   * @tparam Patch type of patch object
   * @return [[io.scalaland.chimney.Patcher]] type class instance
   */
  implicit def derive[T, Patch]: Patcher[T, Patch] =
    macro ChimneyBlackboxMacros.derivePatcherImpl[T, Patch]
}
