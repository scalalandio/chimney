package io.scalaland.chimney.dsl

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, WithRuntimeDataStore}

/** Allows customization of [[io.scalaland.chimney.Patcher]] derivation.
  *
  * @tparam A
  *   type of object to apply patch to
  * @tparam Patch
  *   type of patch object
  * @tparam Overrides
  *   type-level encoded config
  * @tparam Flags
  *   type-level encoded flags
  *
  * @since 0.8.0
  */
final class PatcherDefinition[A, Patch, Overrides <: PatcherOverrides, Flags <: PatcherFlags](
    val runtimeData: PatcherDefinitionCommons.RuntimeDataStore
) extends PatcherFlagsDsl[[Flags1 <: PatcherFlags] =>> PatcherDefinition[A, Patch, Overrides, Flags1], Flags]
    with PatcherDefinitionCommons[
      [Overrides1 <: PatcherOverrides] =>> PatcherDefinition[A, Patch, Overrides1, Flags]
    ]
    with WithRuntimeDataStore {

  /** Build Patcher using current configuration.
    *
    * It runs macro that tries to derive instance of `Patcher[A, Patch]`. When transformation can't be derived, it
    * results with compilation error.
    *
    * @return
    *   [[io.scalaland.chimney.Patcher]] type class instance
    *
    * @since 0.8.0
    */
  inline def buildPatcher[ImplicitScopeFlags <: PatcherFlags](using
      tc: PatcherConfiguration[ImplicitScopeFlags]
  ): Patcher[A, Patch] =
    ${ PatcherMacros.derivePatcherWithConfig[A, Patch, Overrides, Flags, ImplicitScopeFlags]('this) }

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PatcherDefinition(overrideData +: runtimeData).asInstanceOf[this.type]
}
