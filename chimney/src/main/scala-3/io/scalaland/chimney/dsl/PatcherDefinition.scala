package io.scalaland.chimney.dsl

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.compiletime.dsl.PatcherDefinitionMacros
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, Path, WithRuntimeDataStore}

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

  /** Use the `value` provided here for the field picked using the `selectorObj`.
    *
    * By default, if `Patch` is missing a field, the original `A`'s field value is taken.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-patching/#updating-field-with-a-provided-value]] for more details
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 1.7.0
    */
  transparent inline def withFieldConst[T, U](inline selectorObj: A => T, inline value: U)(using
      U <:< T
  ): PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags] =
    ${ PatcherDefinitionMacros.withFieldConstImpl('this, 'selectorObj, 'value) }

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
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 1.7.0
    */
  transparent inline def withFieldComputed[T, U](
      inline selectorObj: A => T,
      inline f: Patch => U
  )(using U <:< T): PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags] =
    ${ PatcherDefinitionMacros.withFieldComputedImpl('this, 'selectorObj, 'f) }

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
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 1.7.0
    */
  transparent inline def withFieldComputedFrom[S, T, U](inline selectorPatch: Patch => S)(
      inline selectorObj: A => T,
      inline f: S => U
  )(using U <:< T): PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags] =
    ${ PatcherDefinitionMacros.withFieldComputedFromImpl('this, 'selectorPatch, 'selectorObj, 'f) }

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
    *   [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 1.7.0
    */
  transparent inline def withFieldIgnored[T](
      inline selectorPatch: Patch => T
  ): PatcherDefinition[A, Patch, ? <: PatcherOverrides, Flags] =
    ${ PatcherDefinitionMacros.withFieldIgnoredImpl('this, 'selectorPatch) }

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
  transparent inline def withPatchedValueFlag[T](
      inline selectorObj: A => T
  ): PatcherPatchedValueFlagsDsl.OfPatcherDefinition[A, Patch, Overrides, Flags, ? <: Path] =
    ${ PatcherDefinitionMacros.withPatchedValueFlagImpl('this, 'selectorObj) }

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
