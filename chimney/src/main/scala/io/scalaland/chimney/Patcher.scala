package io.scalaland.chimney

/** Type class definition that wraps patching behavior.
  *
  * @tparam A type of object to apply patch to
  * @tparam Patch type of patch object
  *
  * @since 0.1.3
  */
trait Patcher[A, Patch] {

  /** Modifies a copy of one object using values from another object.
    *
    * @param obj object to modify
    * @param patch object with modified values
    * @return patched copy
    *
    * @since 0.1.3
    */
  def patch(obj: A, patch: Patch): A
}

/** @since 0.1.3 */
object Patcher extends PatcherCompanionPlatform
