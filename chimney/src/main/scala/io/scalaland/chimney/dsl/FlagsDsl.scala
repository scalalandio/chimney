package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerFlags
import io.scalaland.chimney.internal.TransformerFlags._

trait FlagsDsl[UpdateFlag[_ <: TransformerFlags], Flags <: TransformerFlags] {

  private def enableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Enable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Enable[F, Flags]]]

  private def disableFlag[F <: TransformerFlags.Flag]: UpdateFlag[Disable[F, Flags]] =
    this.asInstanceOf[UpdateFlag[Disable[F, Flags]]]

  /** Enable values to be supplied from method calls. Source method must be public and have no parameter list.
    *
    * By default this is disabled because method calls may perform side effects (e.g. mutations)
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#using-method-accessors]] for more details
    */
  def enableMethodAccessors: UpdateFlag[Enable[MethodAccessors, Flags]] =
    enableFlag[MethodAccessors]

  def disableMethodAccessors: UpdateFlag[Disable[MethodAccessors, Flags]] =
    disableFlag[MethodAccessors]

  /** Fail derivation if `From` type is missing field even if `To` has default value for it.
    *
    * By default in such case derivation will fallback to default values.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#disabling-default-values-in-generated-transformer]] for more details
    */
  def disableDefaultValues: UpdateFlag[Disable[DefaultValues, Flags]] =
    disableFlag[DefaultValues]

  def enableDefaultValues: UpdateFlag[Enable[DefaultValues, Flags]] =
    enableFlag[DefaultValues]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * By default only Scala conversions (`.name`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#reading-from-java-beans]] for more details
    */
  def enableBeanGetters: UpdateFlag[Enable[BeanGetters, Flags]] =
    enableFlag[BeanGetters]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/java-beans.html#writing-to-java-beans]] for more details
    */
  def enableBeanSetters: UpdateFlag[Enable[BeanSetters, Flags]] =
    enableFlag[BeanSetters]

  /** Sets target value of optional field to None if field is missing from source type From.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/default-values.html#default-values-for-option-fields]] for more details
    */
  def enableOptionDefaultsToNone: UpdateFlag[Enable[OptionDefaultsToNone, Flags]] =
    enableFlag[OptionDefaultsToNone]

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
    */
  def enableUnsafeOption: UpdateFlag[Enable[UnsafeOption, Flags]] =
    enableFlag[UnsafeOption]
}
