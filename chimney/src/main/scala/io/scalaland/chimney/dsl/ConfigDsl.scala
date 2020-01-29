package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.TransformerCfg._

trait ConfigDsl[CC[_ <: TransformerCfg], C <: TransformerCfg] {

  /** Fail derivation if `From` type is missing field even if `To` has default value for it.
    *
    * By default in such case derivation will fallback to default values.
    *
    * @see [[https://scalalandio.github.io/chimney/#Defaultoptionvalues]] for more details
    */
  def disableDefaultValues: CC[DisableDefaultValues[C]] =
    this.asInstanceOf[CC[DisableDefaultValues[C]]]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`.
    *
    * By default only Scala conversions (`.name`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/#ReadingfromJavabeans]] for more details
    */
  def enableBeanGetters: CC[EnableBeanGetters[C]] =
    this.asInstanceOf[CC[EnableBeanGetters[C]]]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`.
    *
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/#WritingtoJavabeans]] for more details
    */
  def enableBeanSetters: CC[EnableBeanSetters[C]] =
    this.asInstanceOf[CC[EnableBeanSetters[C]]]

  /** Sets target value of optional field to None if field is missing from source type From.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Defaultoptionvalues]] for more details
    */
  def enableOptionDefaultsToNone: CC[EnableOptionDefaultsToNone[C]] =
    this.asInstanceOf[CC[EnableOptionDefaultsToNone[C]]]

  /** Enable unsafe call to `.get` when source type From contains field of type `Option[A]`,
    * but target type To defines this fields as `A`.
    *
    * It's unsafe as code generated this way may throw at runtime.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Unsafeoption]] for more details
    */
  def enableUnsafeOption: CC[EnableUnsafeOption[C]] =
    this.asInstanceOf[CC[EnableUnsafeOption[C]]]

}
