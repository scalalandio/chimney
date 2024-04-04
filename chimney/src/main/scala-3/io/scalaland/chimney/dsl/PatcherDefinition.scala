package io.scalaland.chimney.dsl

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.compiletime.dsl.*
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides}

/** Allows customization of [[io.scalaland.chimney.Patcher]] derivation.
  *
  * @tparam A         type of object to apply patch to
  * @tparam Patch     type of patch object
  * @tparam Overrides type-level encoded config
  * @tparam Flags     type-level encoded flags
  *
  * @since 0.8.0
  */
final class PatcherDefinition[A, Patch, Overrides <: PatcherOverrides, Flags <: PatcherFlags]
    extends PatcherFlagsDsl[[Flags1 <: PatcherFlags] =>> PatcherDefinition[A, Patch, Overrides, Flags1], Flags] {

  /** Build Patcher using current configuration.
    *
    * It runs macro that tries to derive instance of `Patcher[A, Patch]`. When transformation can't be derived, it
    * results with compilation error.
    *
    * @return [[io.scalaland.chimney.Patcher]] type class instance
    *
    * @since 0.8.0
    */
  inline def buildPatcher[ImplicitScopeFlags <: PatcherFlags](using
      tc: PatcherConfiguration[ImplicitScopeFlags]
  ): Patcher[A, Patch] =
    ${ PatcherDefinitionMacros.buildPatcher[A, Patch, Overrides, Flags, ImplicitScopeFlags] }
}
