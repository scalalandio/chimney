package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerFlags
import io.scalaland.chimney.internal.TransformerFlags.*

import scala.annotation.unused

/** Type-level representation of derivation flags which can be enabled/disabled for a specific transformation or globally.
  *
  * @since 0.6.0
  */
private[dsl] trait FlagsDsl[UpdateFlag[_ <: TransformerFlags], Flags <: TransformerFlags] {

  /** Enable values to be supplied from method calls. Source method must be public and have no parameter list.
    *
    * By default this is disabled because method calls may perform side effects (e.g. mutations)
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#using-method-accessors]] for more details
    *
    * @since 0.6.0
    */
  def enableMethodAccessors: UpdateFlag[Enable[MethodAccessors, Flags]] =
    enableFlag[MethodAccessors]

  /** Disable method accessors lookup that was previously enabled by `enableMethodAccessors`
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#using-method-accessors]] for more details
    *
    * @since 0.5.0
    */
  def disableMethodAccessors: UpdateFlag[Disable[MethodAccessors, Flags]] =
    disableFlag[MethodAccessors]

  /** Enable fallback to default case class values in `To` type.
    *
    * By default in such case derivation will fail. By enabling this flag, derivation will fallback to default value.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#enabling-default-values-in-generated-transformer]] for more details
    *
    * @since 0.6.0
    */
  def enableDefaultValues: UpdateFlag[Enable[DefaultValues, Flags]] =
    enableFlag[DefaultValues]

  /** Fail derivation if `From` type is missing field even if `To` has default value for it.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#enabling-default-values-in-generated-transformer]] for more details
    *
    * @since 0.1.9
    */
  def disableDefaultValues: UpdateFlag[Disable[DefaultValues, Flags]] =
    disableFlag[DefaultValues]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * By default only Scala conversions (`.name`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#reading-from-java-beans]] for more details
    *
    * @since 0.2.1
    */
  def enableBeanGetters: UpdateFlag[Enable[BeanGetters, Flags]] =
    enableFlag[BeanGetters]

  /** Disable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#reading-from-java-beans]] for more details
    *
    * @since 0.6.0
    */
  def disableBeanGetters: UpdateFlag[Disable[BeanGetters, Flags]] =
    disableFlag[BeanGetters]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#writing-to-java-beans]] for more details
    *
    * @since 0.2.1
    */
  def enableBeanSetters: UpdateFlag[Enable[BeanSetters, Flags]] =
    enableFlag[BeanSetters]

  /** Disable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#writing-to-java-beans]] for more details
    *
    * @since 0.6.0
    */
  def disableBeanSetters: UpdateFlag[Disable[BeanSetters, Flags]] =
    disableFlag[BeanSetters]

  /** Sets target value of optional field to None if field is missing from source type `From`.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#default-values-for-option-fields]] for more details
    *
    * @since 0.2.1
    */
  def enableOptionDefaultsToNone: UpdateFlag[Enable[OptionDefaultsToNone, Flags]] =
    enableFlag[OptionDefaultsToNone]

  /** Disable `None` fallback value for optional fields in `To`.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#default-values-for-option-fields]] for more details
    *
    * @since 0.6.0
    */
  def disableOptionDefaultsToNone: UpdateFlag[Disable[OptionDefaultsToNone, Flags]] =
    disableFlag[OptionDefaultsToNone]

  /** Enable unsafe call to `.get` when source type From contains field of type `Option[A]`,
    * but target type To defines this fields as `A`.
    *
    * It's unsafe as code generated this way may throw at runtime.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/unsafe-options.html]] for more details
    *
    * @since 0.3.3
    */
  @deprecated("Unsafe options are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def enableUnsafeOption: UpdateFlag[Enable[UnsafeOption, Flags]] =
    enableFlag[UnsafeOption]

  /** Disable unsafe value extraction from optional fields in `From` type.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/unsafe-options.html]] for more details
    *
    * @since 0.6.0
    */
  @deprecated("Unsafe options are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def disableUnsafeOption: UpdateFlag[Disable[UnsafeOption, Flags]] =
    disableFlag[UnsafeOption]

  /** Enable conflict resolution when both `Transformer` and `PartialTransformer` are available in the implicit scope.
    *
    * @param preference parameter specifying which implicit transformer to pick in case of conflict
    *
    * @see [[https://scalalandio.github.io/chimney/partial-transformers/total-vs-partial-conflicts.html]] for more details
    *
    * @since 0.7.0
    */
  def enableImplicitConflictResolution[P <: ImplicitTransformerPreference](
      @unused preference: P
  ): UpdateFlag[Enable[ImplicitConflictResolution[P], Flags]] =
    enableFlag[ImplicitConflictResolution[P]]

  /** Disable any implicit conflict resolution preference that was set previously.
    *
    * @see [[https://scalalandio.github.io/chimney/partial-transformers/total-vs-partial-conflicts.html]] for more details
    *
    * @since 0.7.0
    */
  def disableImplicitConflictResolution: UpdateFlag[Disable[ImplicitConflictResolution[?], Flags]] =
    disableFlag[ImplicitConflictResolution[?]]

  private def enableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Enable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Enable[F, Flags]]]

  private def disableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Disable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Disable[F, Flags]]]
}
