package io.scalaland.chimney.dsl

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.runtime.{PatcherCfg, PatcherFlags}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.Patcher]] derivation.
  *
  * @tparam A     type of object to apply patch to
  * @tparam Patch type of patch object
  * @tparam Cfg   type-level encoded config
  * @tparam Flags type-level encoded flags
  *
  * @since 0.8.0
  */
final class PatcherDefinition[From, To, Cfg <: PatcherCfg, Flags <: PatcherFlags]
    extends PatcherFlagsDsl[Lambda[
      `Flags1 <: PatcherFlags` => PatcherDefinition[From, To, Cfg, Flags1]
    ], Flags] {

  /** Build Patcher using current configuration.
    *
    * It runs macro that tries to derive instance of `Patcher[From, To]`.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[io.scalaland.chimney.Patcher]] type class instance
    *
    * @since 0.8.0
    */
  def buildPatcher[ImplicitScopeFlags <: PatcherFlags](implicit
      tc: io.scalaland.chimney.dsl.PatcherConfiguration[ImplicitScopeFlags]
  ): Patcher[From, To] =
    macro PatcherMacros.derivePatcherWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags]
}
