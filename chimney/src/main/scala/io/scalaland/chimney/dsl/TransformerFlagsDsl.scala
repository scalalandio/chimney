package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.TransformerFlags.*
import io.scalaland.chimney.internal.runtime.TransformerFlags

import scala.annotation.unused

/** Type-level representation of derivation flags which can be enabled/disabled for a specific transformation or
  * globally.
  *
  * @since 0.6.0
  */
private[dsl] trait TransformerFlagsDsl[UpdateFlag[_ <: TransformerFlags], Flags <: TransformerFlags] {

  /** Enable lookup in definitions inherited from supertype.
    *
    * By default only values defined directly in the type are considered. With this flag supertype methods would not be
    * filtered out
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#reading-from-inherited-valuesmethods]] for more
    *   details
    *
    * @since 0.8.0
    */
  def enableInheritedAccessors: UpdateFlag[Enable[InheritedAccessors, Flags]] =
    enableFlag[InheritedAccessors]

  /** Disable inherited accessors lookup that was previously enabled by `enableInheritedAccessors`
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#reading-from-inherited-valuesmethods]] for more
    *   details
    *
    * @since 0.8.0
    */
  def disableInheritedAccessors: UpdateFlag[Disable[InheritedAccessors, Flags]] =
    disableFlag[InheritedAccessors]

  /** Enable values to be supplied from method calls. Source method must be public and have no parameter list.
    *
    * By default this is disabled because method calls may perform side effects (e.g. mutations)
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#reading-from-methods]] for more details
    *
    * @since 0.6.0
    */
  def enableMethodAccessors: UpdateFlag[Enable[MethodAccessors, Flags]] =
    enableFlag[MethodAccessors]

  /** Disable method accessors lookup that was previously enabled by `enableMethodAccessors`
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#reading-from-methods]] for more details
    *
    * @since 0.5.0
    */
  def disableMethodAccessors: UpdateFlag[Disable[MethodAccessors, Flags]] =
    disableFlag[MethodAccessors]

  /** Enable fallback to default case class values in `To` type.
    *
    * By default in such case derivation will fail. By enabling this flag, derivation will fallback to default value.
    *
    * This flag can be set in parallel to enabling default values for specific field type with
    * [[enableDefaultValueOfType]].
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#allowing-fallback-to-the-constructors-default-values]]
    *   for more details
    *
    * @since 0.6.0
    */
  def enableDefaultValues: UpdateFlag[Enable[DefaultValues, Flags]] =
    enableFlag[DefaultValues]

  /** Fail derivation if `From` type is missing field even if `To` has default value for it.
    *
    * This flag can be set in parallel to enabling default values for specific field type with *
    * [[disableDefaultValueOfType]].
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#allowing-fallback-to-the-constructors-default-values]]
    *   for more details
    *
    * @since 0.1.9
    */
  def disableDefaultValues: UpdateFlag[Disable[DefaultValues, Flags]] =
    disableFlag[DefaultValues]

  /** Enable fallback to default case class values in `To` type for fields of `T` type.
    *
    * By default in such case derivation will fail. By enabling this flag, derivation will fallback to default value.
    *
    * This flag can be set in parallel to globally enabling default values with [[enableDefaultValues]].
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#allowing-fallback-to-the-constructors-default-values]]
    *   for more details
    *
    * @since 1.2.0
    */
  def enableDefaultValueOfType[T]: UpdateFlag[Enable[DefaultValueOfType[T], Flags]] =
    enableFlag[DefaultValueOfType[T]]

  /** Fail derivation if `From` type is missing field even if `To` has default value type for fields of `T` type.
    *
    * This flag can be set in parallel to globally enabling default values with [[disableDefaultValues]].
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#allowing-fallback-to-the-constructors-default-values]]
    *   for more details
    *
    * @since 1.2.0
    */
  def disableDefaultValueOfType[T]: UpdateFlag[Disable[DefaultValueOfType[T], Flags]] =
    disableFlag[DefaultValueOfType[T]]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * By default only Scala conversions (`.name`) are allowed.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#reading-from-bean-getters]] for more details
    *
    * @since 0.2.1
    */
  def enableBeanGetters: UpdateFlag[Enable[BeanGetters, Flags]] =
    enableFlag[BeanGetters]

  /** Disable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#reading-from-bean-getters]] for more details
    *
    * @since 0.6.0
    */
  def disableBeanGetters: UpdateFlag[Disable[BeanGetters, Flags]] =
    disableFlag[BeanGetters]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#writing-to-bean-setters]] for more details
    *
    * @since 0.2.1
    */
  def enableBeanSetters: UpdateFlag[Enable[BeanSetters, Flags]] =
    enableFlag[BeanSetters]

  /** Disable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#writing-to-bean-setters]] for more details
    *
    * @since 0.6.0
    */
  def disableBeanSetters: UpdateFlag[Disable[BeanSetters, Flags]] =
    disableFlag[BeanSetters]

  /** Enable not failing compilation on unresolved Java Beans naming convention (`.setName(value)`) in `To`.
    *
    * By default presence of setters (`.setName(value)`) fails compilation unless setters are enabled and matched with a
    * source field or provided valued.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#ignoring-unmatched-bean-setters]] for more details
    *
    * @since 0.8.3
    */
  def enableIgnoreUnmatchedBeanSetters: UpdateFlag[Enable[BeanSettersIgnoreUnmatched, Flags]] =
    enableFlag[BeanSettersIgnoreUnmatched]

  /** Disable not failing compilation on unresolved Java Beans naming convention (`.setName(value)`) in `To`.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#ignoring-unmatched-bean-setters]] for more details
    *
    * @since 0.8.3
    */
  def disableIgnoreUnmatchedBeanSetters: UpdateFlag[Disable[BeanSettersIgnoreUnmatched, Flags]] =
    disableFlag[BeanSettersIgnoreUnmatched]

  /** Enable calling unary non-Unit methods with Java Beans naming convention (`.setName(value)`) in `To`.
    *
    * By default only methods returning `Unit` (`setName(value): Unit`) could be considered setters.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#writing-to-non-unit-bean-setters]] for more details
    *
    * @since 1.0.0
    */
  def enableNonUnitBeanSetters: UpdateFlag[Enable[NonUnitBeanSetters, Flags]] =
    enableFlag[NonUnitBeanSetters]

  /** Enable calling unary non-Unit methods with Java Beans naming convention (`.setName(value)`) in `To`.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#writing-to-non-unit-bean-setters]] for more details
    *
    * @since 1.0.0
    */
  def disableNonUnitBeanSetters: UpdateFlag[Disable[NonUnitBeanSetters, Flags]] =
    disableFlag[NonUnitBeanSetters]

  /** Sets target value of optional field to None if field is missing from source type `From`.
    *
    * By default in such case compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#allowing-fallback-to-none-as-the-constructors-argument]]
    *   for more details
    *
    * @since 0.2.1
    */
  def enableOptionDefaultsToNone: UpdateFlag[Enable[OptionDefaultsToNone, Flags]] =
    enableFlag[OptionDefaultsToNone]

  /** Disable `None` fallback value for optional fields in `To`.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#allowing-fallback-to-none-as-the-constructors-argument]]
    *   for more details
    *
    * @since 0.6.0
    */
  def disableOptionDefaultsToNone: UpdateFlag[Disable[OptionDefaultsToNone, Flags]] =
    disableFlag[OptionDefaultsToNone]

  /** Enable safe Option unwrapping by `PartialTransformer` - `Option` is automatically unwrapped to non-`Option`
    * values, `None` is treated as empty value errors.
    *
    * This is the default behavior.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#controlling-automatic-option-unwrapping]] for more
    *   details
    *
    * @since 1.0.0
    */
  def enablePartialUnwrapsOption: UpdateFlag[Enable[PartialUnwrapsOption, Flags]] =
    enableFlag[PartialUnwrapsOption]

  /** Disable safe `Option` unwrapping by `PartialTransformer` - each `Option` to non-Option` has to be handled
    * manually.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#controlling-automatic-option-unwrapping]] for more
    *   details
    *
    * @since 1.0.0
    */
  def disablePartialUnwrapsOption: UpdateFlag[Disable[PartialUnwrapsOption, Flags]] =
    disableFlag[PartialUnwrapsOption]

  /** Enable unpacking/wrapping with wrapper types (classes with have only 1 val, set in a constructor) even when they
    * are not AnyVals.
    *
    * By default in such case compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#frominto-a-wrapper-type]] for more details
    *
    * @since 1.3.0
    */
  def enableNonAnyValWrappers: UpdateFlag[Enable[NonAnyValWrappers, Flags]] =
    enableFlag[NonAnyValWrappers]

  /** Disable unpacking/wrapping with wrapper types (classes with have only 1 val, set in a constructor) even when they
    * are not AnyVals.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#frominto-a-wrapper-type]] for more details
    *
    * @since 1.3.0
    */
  def disableNonAnyValWrappers: UpdateFlag[Disable[NonAnyValWrappers, Flags]] =
    disableFlag[NonAnyValWrappers]

  /** Enable conflict resolution when both `Transformer` and `PartialTransformer` are available in the implicit scope.
    *
    * @param preference
    *   parameter specifying which implicit transformer to pick in case of conflict
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#resolving-priority-of-implicit-total-vs-partial-transformers]]
    *   for more details
    *
    * @since 0.7.0
    */
  def enableImplicitConflictResolution[P <: ImplicitTransformerPreference](
      @unused preference: P
  ): UpdateFlag[Enable[ImplicitConflictResolution[P], Flags]] =
    enableFlag[ImplicitConflictResolution[P]]

  /** Disable any implicit conflict resolution preference that was set previously.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#resolving-priority-of-implicit-total-vs-partial-transformers]]
    *   for more details
    *
    * @since 0.7.0
    */
  def disableImplicitConflictResolution: UpdateFlag[Disable[ImplicitConflictResolution[?], Flags]] =
    disableFlag[ImplicitConflictResolution[?]]

  /** Enable custom way of comparing if source fields' names and target fields' names are matching.
    *
    * @param namesComparison
    *   parameter specifying how names should be compared by macro
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#customizing-field-name-matching]] for more details
    *
    * @since 1.0.0
    */
  def enableCustomFieldNameComparison[C <: TransformedNamesComparison & Singleton](
      @unused namesComparison: C
  ): UpdateFlag[Enable[FieldNameComparison[C], Flags]] =
    enableFlag[FieldNameComparison[C]]

  /** Disable any custom way of comparing if source fields' names and target fields' names are matching.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#customizing-field-name-matching]] for more details
    *
    * @since 1.0.0
    */
  def disableCustomFieldNameComparison: UpdateFlag[Disable[FieldNameComparison[?], Flags]] =
    disableFlag[FieldNameComparison[?]]

  /** Enable custom way of comparing if source subtypes' names and target fields' names are matching.
    *
    * @param namesComparison
    *   parameter specifying how names should be compared by macro
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#customizing-subtype-name-matching]] for more details
    *
    * @since 1.0.0
    */
  def enableCustomSubtypeNameComparison[C <: TransformedNamesComparison & Singleton](
      @unused namesComparison: C
  ): UpdateFlag[Enable[SubtypeNameComparison[C], Flags]] =
    enableFlag[SubtypeNameComparison[C]]

  /** Disable any custom way of comparing if source subtypes' names and target fields' names are matching.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#customizing-subtype-name-matching]] for more details
    *
    * @since 1.0.0
    */
  def disableCustomSubtypeNameComparison: UpdateFlag[Disable[SubtypeNameComparison[?], Flags]] =
    disableFlag[SubtypeNameComparison[?]]

  /** Enable printing the logs from the derivation process.
    *
    * @see
    *   [[https://chimney.readthedocs.io/troubleshooting/#debugging-macros]] for more details
    *
    * @since 0.8.0
    */
  def enableMacrosLogging: UpdateFlag[Enable[MacrosLogging, Flags]] =
    enableFlag[MacrosLogging]

  /** Disable printing the logs from the derivation process.
    *
    * @see
    *   [[https://chimney.readthedocs.io/troubleshooting/#debugging-macros]] for more details
    *
    * @since 0.8.0
    */
  def disableMacrosLogging: UpdateFlag[Disable[MacrosLogging, Flags]] =
    disableFlag[MacrosLogging]

  /** Enable an action to be executed upon unused fields in the source type.
    *
    * @param action
    *   parameter specifying what to do when some fields of the source are not used in the target
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#unused-source-fields-policies]] for more details for more details
   *
    * @since 1.5.0
    */
  def enableUnusedFieldPolicy[R <: ActionOnUnused](@unused action: R): UpdateFlag[Enable[UnusedFieldPolicy[R], Flags]] =
    enableFlag[UnusedFieldPolicy[R]]

  /** Disable any action registered to be executed upon unused fields in the source type.
    *
    * @see
    *   [[https://chimney.readthedocs.io/TODO:???]] for more details for more details
    * @since 1.5.0
    */
  def disableUnusedFieldPolicy: UpdateFlag[Disable[UnusedFieldPolicy[?], Flags]] =
    disableFlag[UnusedFieldPolicy[?]]

  private def enableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Enable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Enable[F, Flags]]]

  private def disableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Disable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Disable[F, Flags]]]
}
