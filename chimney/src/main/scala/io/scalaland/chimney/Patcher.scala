package io.scalaland.chimney

/** Type class definition that wraps patching behavior.
  *
  * @tparam T type of object to apply patch to
  * @tparam Patch type of patch object
  *
  * @since 0.1.3
  */
trait Patcher[T, Patch] {

  /** Modifies a copy of one object using values from another object.
    *
    * @param obj object to modify
    * @param patch object with modified values
    * @return patched copy
    *
    * @since 0.1.3
    */
  def patch(obj: T, patch: Patch): T
}

/** @since 0.1.3 */
object Patcher extends PatcherCompanionPlatform
