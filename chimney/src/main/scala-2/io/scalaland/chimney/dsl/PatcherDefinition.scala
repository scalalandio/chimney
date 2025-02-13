package io.scalaland.chimney.dsl

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.compiletime.dsl.PatcherDefinitionMacros
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, Path, WithRuntimeDataStore}

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
final class PatcherDefinition[A, Patch, Overrides <: PatcherOverrides, Flags <: PatcherFlags](
    val runtimeData: PatcherDefinitionCommons.RuntimeDataStore
) extends PatcherFlagsDsl[Lambda[
      `Flags1 <: PatcherFlags` => PatcherDefinition[A, Patch, Overrides, Flags1]
    ], Flags]
    with PatcherDefinitionCommons[
      Lambda[`Overrides1 <: PatcherOverrides` => PatcherDefinition[A, Patch, Overrides1, Flags]]
    ]
    with WithRuntimeDataStore {

  /** Use the `value` provided here for the field picked using the `selectorObj`.
    *
    * By default, if `Patch` is missing a field, the original `A`'s field value is taken.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#updating-field-with-a-provided-value]] for more details
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherDefinition]]
    *
    * @since 1.7.0
    */
  def withFieldConst[T, U](selectorObj: A => T, value: U)(implicit
      ev: U <:< T
  ): PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags] =
    macro PatcherDefinitionMacros.withFieldConstImpl[A, Patch, Overrides, Flags]

  /** Use the function `f` to compute a value of the field picked using the `selectorObj`.
    *
    * By default, if `Patch` is missing a field, the original `A`'s field value is taken.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#updating-field-with-a-computed-value]] for more details
    *
    * @tparam T
    *   type of patched value field
    * @tparam U
    *   type of computed value
    * @param selectorObj
    *   patched value field in `A`, defined like `_.name`
    * @param f
    *   function used to compute value of the target field
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherDefinition]]
    *
    * @since 1.7.0
    */
  def withFieldComputed[T, U](
      selectorObj: A => T,
      f: Patch => U
  )(implicit ev: U <:< T): PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags] =
    macro PatcherDefinitionMacros.withFieldComputedImpl[A, Patch, Overrides, Flags]

  /** Use the function `f` to compute a value of the field picked using the `selectorObj` from a value extracted with
    * `selectorPatch` as an input.
    *
    * By default, if `Patch` is missing a field, the original `A`'s field value is taken.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#updating-field-with-a-computed-value]] for more details
    *
    * @tparam S
    *   type of patch field
    * @tparam T
    *   type of patched value field
    * @tparam U
    *   type of computed value
    * @param selectorPatch
    *   patch field in `Patch`, defined like `_.name`
    * @param selectorObj
    *   patched value field in `A`, defined like `_.name`
    * @param f
    *   function used to compute value of the target field
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherDefinition]]
    *
    * @since 1.7.0
    */
  def withFieldComputedFrom[S, T, U](selectorPatch: Patch => S)(
      selectorObj: A => T,
      f: S => U
  )(implicit ev: U <:< T): PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags] =
    macro PatcherDefinitionMacros.withFieldComputedFromImpl[A, Patch, Overrides, Flags]

  /** Mark `Patch`` field as expected to be ignored, so that the orignial value would be used.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#ignoring-fields-in-patches]] for more details
    *
    * @tparam T
    *   type of patch field
    * @param selectorPatch
    *   patch field in `Patch`, defined like `_.originalName`
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherDefinition]]
    *
    * @since 1.7.0
    */
  def withFieldIgnored[T](selectorPatch: Patch => T): PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags] =
    macro PatcherDefinitionMacros.withFieldIgnoredImpl[A, Patch, Overrides, Flags]

  /** Define a flag only on some source value using the `selectorTo`, rather than globally.
    *
    * @see
    *   [[https://chimney.readthedocs.io/cookbook/#constraining-flags-to-a-specific-fieldsubtype]] for more details
    *
    * @tparam T
    *   type of the target field
    * @param selectorObj
    *   patched value field in `A`, defined like `_.name`
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherPatchedValueFlagsDsl.OfPatcherDefinition]]
    *
    * @since 1.7.0
    */
  def withPatchedValueFlag[T](
      selectorObj: A => T
  ): PatcherPatchedValueFlagsDsl.OfPatcherDefinition[A, Patch, Overrides, Flags, ? <: Path] =
    macro PatcherDefinitionMacros.withPatchedValueFlagImpl[A, Patch, Overrides, Flags]

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
  ): Patcher[A, Patch] =
    macro PatcherMacros.derivePatcherWithConfig[A, Patch, Overrides, Flags, ImplicitScopeFlags]

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PatcherDefinition(overrideData +: runtimeData).asInstanceOf[this.type]
}
