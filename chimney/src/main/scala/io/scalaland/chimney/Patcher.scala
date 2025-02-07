package io.scalaland.chimney

import io.scalaland.chimney.dsl.{PatcherDefinition, PatcherDefinitionCommons}
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides}

/** Type class definition that wraps patching behavior.
  *
  * @note
  *   You should not need to instantiate this class manually, if you can derive it - take a look at [[.derive]] and
  *   [[.define]] methods for that. Manual intantiation is only necessary if you want to add support for the
  *   transformation that is not supported out of the box. Even then consult
  *   [[https://chimney.readthedocs.io/cookbook/#integrations]] first!
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-patching/]]
  *
  * @tparam A
  *   type of object to apply patch to
  * @tparam Patch
  *   type of patch object
  *
  * @since 0.1.3
  */
@FunctionalInterface
trait Patcher[A, Patch] extends Patcher.AutoDerived[A, Patch] {

  /** Modifies a copy of one object using values from another object.
    *
    * @param obj
    *   object to modify
    * @param patch
    *   object with modified values
    * @return
    *   patched copy
    *
    * @since 0.1.3
    */
  def patch(obj: A, patch: Patch): A
}

/** Companion of [[io.scalaland.chimney.Patcher]].
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-patching/]]
  *
  * @since 0.1.3
  */
object Patcher extends PatcherCompanionPlatform {

  /** Creates an empty [[io.scalaland.chimney.dsl.PatcherDefinition]] that you can customize to derive
    * [[io.scalaland.chimney.Patcher]].
    *
    * @see
    *   [[io.scalaland.chimney.dsl.PatcherDefinition]] for available settings
    *
    * @tparam A
    *   type of object to apply patch to
    * @tparam Patch
    *   type of patch object
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherDefinition]] with defaults
    *
    * @since 0.8.0
    */
  def define[A, Patch]: PatcherDefinition[A, Patch, PatcherOverrides.Empty, PatcherFlags.Default] =
    new PatcherDefinition(PatcherDefinitionCommons.emptyRuntimeDataStore)

  /** Type class used when you want to allow using automatically derived patchings.
    *
    * When we want to only allow semiautomatically derived/manually defined instances you should use
    * [[io.scalaland.chimney.Patcher]].
    *
    * @see
    *   [[https://chimney.readthedocs.io/cookbook/#automatic-semiautomatic-and-inlined-derivation]] for more details
    *
    * @tparam A
    *   type of object to apply patch to
    * @tparam Patch
    *   type of patch object
    *
    * @since 0.8.0
    */
  @FunctionalInterface
  trait AutoDerived[A, Patch] {
    def patch(obj: A, patch: Patch): A
  }

  /** @since 0.8.0 */
  object AutoDerived extends PatcherAutoDerivedCompanionPlatform
}
