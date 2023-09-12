package io.scalaland.chimney

import io.scalaland.chimney.dsl.PatcherDefinition
import io.scalaland.chimney.internal.runtime.{PatcherCfg, PatcherFlags}

/** Type class definition that wraps patching behavior.
  *
  * @tparam A type of object to apply patch to
  * @tparam Patch type of patch object
  *
  * @since 0.1.3
  */
trait Patcher[A, Patch] extends Patcher.AutoDerived[A, Patch] {

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
object Patcher extends PatcherCompanionPlatform {

  /** Creates an empty [[io.scalaland.chimney.dsl.PatcherDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.Patcher]].
    *
    * @see [[io.scalaland.chimney.dsl.PatcherDefinition]] for available settings
    * @tparam A     type of object to apply patch to
    * @tparam Patch type of patch object
    * @return [[io.scalaland.chimney.dsl.PatcherDefinition]] with defaults
    *
    * @since 0.8.0
    */
  def define[A, Patch]: PatcherDefinition[A, Patch, PatcherCfg.Empty, PatcherFlags.Default] =
    new PatcherDefinition

  trait AutoDerived[A, Patch] {
    def patch(obj: A, patch: Patch): A
  }
}
