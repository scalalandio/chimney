package io.scalaland.chimney.dsl

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, WithRuntimeDataStore}

import scala.language.experimental.macros

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
final class PatcherDefinition[From, To, Overrides <: PatcherOverrides, Flags <: PatcherFlags](
    val runtimeData: PatcherDefinitionCommons.RuntimeDataStore
) extends PatcherFlagsDsl[Lambda[
      `Flags1 <: PatcherFlags` => PatcherDefinition[From, To, Overrides, Flags1]
    ], Flags]
    with PatcherDefinitionCommons[
      Lambda[`Overrides1 <: PatcherOverrides` => PatcherDefinition[From, To, Overrides1, Flags]]
    ]
    with WithRuntimeDataStore {

  /** Build Patcher using current configuration.
    *
    * It runs macro that tries to derive instance of `Patcher[From, To]`. When transformation can't be derived, it
    * results with compilation error.
    *
    * @return
    *   [[io.scalaland.chimney.Patcher]] type class instance
    *
    * @since 0.8.0
    */
  def buildPatcher[ImplicitScopeFlags <: PatcherFlags](implicit
      pc: io.scalaland.chimney.dsl.PatcherConfiguration[ImplicitScopeFlags]
  ): Patcher[From, To] =
    macro PatcherMacros.derivePatcherWithConfig[From, To, Overrides, Flags, ImplicitScopeFlags]

  // FIXME: (2.0.0 cleanup) - kept to make MiMa happy
  // $COVERAGE-OFF$
  def this() = this(PatcherDefinitionCommons.emptyRuntimeDataStore)
  // $COVERAGE-ON$

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PatcherDefinition(overrideData +: runtimeData).asInstanceOf[this.type]
}
