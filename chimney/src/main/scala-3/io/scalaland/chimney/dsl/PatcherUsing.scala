package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.compiletime.dsl.PatcherUsingMacros
import io.scalaland.chimney.internal.runtime.{PatcherFlags, PatcherOverrides, Path, WithRuntimeDataStore}

/** Provides operations to customize [[io.scalaland.chimney.Patcher]] logic for specific object value and patch value.
  *
  * @tparam A
  *   type of object to apply patch to
  * @tparam Patch
  *   type of patch object
  * @tparam Overrides
  *   type-level encoded config
  * @tparam Flags
  *   type-level encoded flags
  * @param obj
  *   object to patch
  * @param objPatch
  *   patch object
  * @param pd
  *   patcher definition
  *
  * @since 0.4.0
  */
final class PatcherUsing[A, Patch, Overrides <: PatcherOverrides, Flags <: PatcherFlags](
    val obj: A,
    val objPatch: Patch,
    val pd: PatcherDefinition[A, Patch, Overrides, Flags]
) extends PatcherFlagsDsl[[Flags1 <: PatcherFlags] =>> PatcherUsing[A, Patch, Overrides, Flags1], Flags]
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
  transparent inline def withFieldConst[T, U](inline selectorObj: A => T, value: U)(using
      U <:< T
  ): PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags] =
    ${ PatcherUsingMacros.withFieldConstImpl('this, 'selectorObj, 'value) }

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
  transparent inline def withFieldComputed[T, U](
      inline selectorObj: A => T,
      inline f: Patch => U
  )(using U <:< T): PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags] =
    ${ PatcherUsingMacros.withFieldComputedImpl('this, 'selectorObj, 'f) }

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
  )(using U <:< T): PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags] =
    ${ PatcherUsingMacros.withFieldComputedFromImpl('this, 'selectorPatch, 'selectorObj, 'f) }

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
  ): PatcherUsing[A, Patch, ? <: PatcherOverrides, Flags] =
    ${ PatcherUsingMacros.withFieldIgnoredImpl('this, 'selectorPatch) }

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
    *   [[io.scalaland.chimney.dsl.PatcherPatchedValueFlagsDsl.OfPatcherUsing]]
    *
    * @since 1.7.0
    */
  transparent inline def withPatchedValueFlag[T](
      inline selectorObj: A => T
  ): PatcherPatchedValueFlagsDsl.OfPatcherUsing[A, Patch, Overrides, Flags, ? <: Path] =
    ${ PatcherUsingMacros.withPatchedValueFlagImpl('this, 'selectorObj) }

  /** Applies configured patching in-place.
    *
    * @return
    *   patched value
    *
    * @since 0.4.0
    */
  inline def patch[ImplicitScopeFlags <: PatcherFlags](using
      tc: PatcherConfiguration[ImplicitScopeFlags]
  ): A =
    ${
      PatcherMacros.derivePatcherResultWithConfig[A, Patch, Overrides, Flags, ImplicitScopeFlags]('obj, 'objPatch, 'pd)
    }

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PatcherUsing(obj, objPatch, pd.addOverride(overrideData)).asInstanceOf[this.type]
}
