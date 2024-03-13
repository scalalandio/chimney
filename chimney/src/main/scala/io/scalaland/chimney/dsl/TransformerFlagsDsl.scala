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
    * @see [[https://chimney.readthedocs.io/supported-transformations/#reading-from-inherited-valuesmethods]] for more details
    *
    * @since 0.8.0
    */
  def enableInheritedAccessors: UpdateFlag[Enable[InheritedAccessors, Flags]] =
    enableFlag[InheritedAccessors]

  /** Disable inherited accessors lookup that was previously enabled by `enableInheritedAccessors`
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#reading-from-inherited-valuesmethods]] for more details
    *
    * @since 0.8.0
    */
  def disableInheritedAccessors: UpdateFlag[Disable[InheritedAccessors, Flags]] =
    disableFlag[InheritedAccessors]

  /** Enable values to be supplied from method calls. Source method must be public and have no parameter list.
    *
    * By default this is disabled because method calls may perform side effects (e.g. mutations)
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#reading-from-methods]] for more details
    *
    * @since 0.6.0
    */
  def enableMethodAccessors: UpdateFlag[Enable[MethodAccessors, Flags]] =
    enableFlag[MethodAccessors]

  /** Disable method accessors lookup that was previously enabled by `enableMethodAccessors`
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#reading-from-methods]] for more details
    *
    * @since 0.5.0
    */
  def disableMethodAccessors: UpdateFlag[Disable[MethodAccessors, Flags]] =
    disableFlag[MethodAccessors]

  /** Enable fallback to default case class values in `To` type.
    *
    * By default in such case derivation will fail. By enabling this flag, derivation will fallback to default value.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#allowing-fallback-to-the-constructors-default-values]] for more details
    *
    * @since 0.6.0
    */
  def enableDefaultValues: UpdateFlag[Enable[DefaultValues, Flags]] =
    enableFlag[DefaultValues]

  /** Fail derivation if `From` type is missing field even if `To` has default value for it.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#allowing-fallback-to-the-constructors-default-values]] for more details
    *
    * @since 0.1.9
    */
  def disableDefaultValues: UpdateFlag[Disable[DefaultValues, Flags]] =
    disableFlag[DefaultValues]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * By default only Scala conversions (`.name`) are allowed.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#reading-from-bean-getters]] for more details
    *
    * @since 0.2.1
    */
  def enableBeanGetters: UpdateFlag[Enable[BeanGetters, Flags]] =
    enableFlag[BeanGetters]

  /** Disable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#reading-from-bean-getters]] for more details
    *
    * @since 0.6.0
    */
  def disableBeanGetters: UpdateFlag[Disable[BeanGetters, Flags]] =
    disableFlag[BeanGetters]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#writing-to-bean-setters]] for more details
    *
    * @since 0.2.1
    */
  def enableBeanSetters: UpdateFlag[Enable[BeanSetters, Flags]] =
    enableFlag[BeanSetters]

  /** Disable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#writing-to-bean-setters]] for more details
    *
    * @since 0.6.0
    */
  def disableBeanSetters: UpdateFlag[Disable[BeanSetters, Flags]] =
    disableFlag[BeanSetters]

  /** Enable not failing compilation on unresolved Java Beans naming convention (`.setName(value)`) in `To`.
    *
    * By default presence of setters (`.setName(value)`) fails compilation unless setters are enabled and matched with
    * a source field or provided valued.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#ignoring-unmatched-bean-setters]] for more details
    *
    * @since 0.8.3
    */
  def enableIgnoreUnmatchedBeanSetters: UpdateFlag[Enable[BeanSettersIgnoreUnmatched, Flags]] =
    enableFlag[BeanSettersIgnoreUnmatched]

  /** Disable not failing compilation on unresolved Java Beans naming convention (`.setName(value)`) in `To`.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#wignoring-unmatched-bean-setters]] for more details
    *
    * @since 0.8.3
    */
  def disableIgnoreUnmatchedBeanSetters: UpdateFlag[Disable[BeanSettersIgnoreUnmatched, Flags]] =
    disableFlag[BeanSettersIgnoreUnmatched]

  /** Sets target value of optional field to None if field is missing from source type `From`.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#allowing-fallback-to-none-as-the-constructors-argument]] for more details
    *
    * @since 0.2.1
    */
  def enableOptionDefaultsToNone: UpdateFlag[Enable[OptionDefaultsToNone, Flags]] =
    enableFlag[OptionDefaultsToNone]

  /** Disable `None` fallback value for optional fields in `To`.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#allowing-fallback-to-none-as-the-constructors-argument]] for more details
    *
    * @since 0.6.0
    */
  def disableOptionDefaultsToNone: UpdateFlag[Disable[OptionDefaultsToNone, Flags]] =
    disableFlag[OptionDefaultsToNone]

  /** Enable safe Option unwrapping by `PartialTransformer` - `Option` is automatically unwrapped to non-`Option` values, `None` is treated as empty value errors.
    *
    * This is the default behavior.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#TODO]] for more details
    *
    * @since 1.0.0
    */
  def enablePartialUnwrapsOption: UpdateFlag[Enable[PartialUnwrapsOption, Flags]] =
    enableFlag[PartialUnwrapsOption]

  /** Disable safe `Option` unwrapping by `PartialTransformer` - each `Option` to non-Option` has to be handled manually.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#TODO]] for more details
    *
    * @since 1.0.0
    */
  def disablePartialUnwrapsOption: UpdateFlag[Disable[PartialUnwrapsOption, Flags]] =
    disableFlag[PartialUnwrapsOption]

  /** Enable conflict resolution when both `Transformer` and `PartialTransformer` are available in the implicit scope.
    *
    * @param preference parameter specifying which implicit transformer to pick in case of conflict
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#resolving-priority-of-implicit-total-vs-partial-transformers]] for more details
    *
    * @since 0.7.0
    */
  def enableImplicitConflictResolution[P <: ImplicitTransformerPreference](
      @unused preference: P
  ): UpdateFlag[Enable[ImplicitConflictResolution[P], Flags]] =
    enableFlag[ImplicitConflictResolution[P]]

  /** Disable any implicit conflict resolution preference that was set previously.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#resolving-priority-of-implicit-total-vs-partial-transformers]] for more details
    *
    * @since 0.7.0
    */
  def disableImplicitConflictResolution: UpdateFlag[Disable[ImplicitConflictResolution[?], Flags]] =
    disableFlag[ImplicitConflictResolution[?]]

  /** Enable printing the logs from the derivation process.
    *
    * @see [[https://chimney.readthedocs.io/troubleshooting/#debugging-macros]] for more details
    *
    * @since 0.8.0
    */
  def enableMacrosLogging: UpdateFlag[Enable[MacrosLogging, Flags]] =
    enableFlag[MacrosLogging]

  /** Disable printing the logs from the derivation process.
    *
    * @see [[https://chimney.readthedocs.io/troubleshooting/#debugging-macros]] for more details
    *
    * @since 0.8.0
    */
  def disableMacrosLogging: UpdateFlag[Disable[MacrosLogging, Flags]] =
    disableFlag[MacrosLogging]

  private def enableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Enable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Enable[F, Flags]]]

  private def disableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Disable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Disable[F, Flags]]]
}
