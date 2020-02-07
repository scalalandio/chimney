package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.{ChimneyBlackboxMacros, TransformerIntoWhiteboxMacros}

import scala.language.experimental.macros

/** Provides DSL for configuring [[io.scalaland.chimney.Transformer]]'s
  * generation and using the result to transform value at the same time
  *
  * @param  source object to transform
  * @param  td     transformer definition
  * @tparam From   type of input value
  * @tparam To     type of output value
  * @tparam C      type-level encoded config
  */
final class TransformerInto[From, To, C <: TransformerCfg](
    val source: From,
    val td: TransformerDefinition[From, To, C]
) {

  /** Fail derivation if `From` type is missing field even if `To` has default value for it
    *
    * By default in such case derivation will fallback to default values.
    *
    * @see [[https://scalalandio.github.io/chimney/#Defaultoptionvalues]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  def disableDefaultValues: TransformerInto[From, To, DisableDefaultValues[C]] =
    this.asInstanceOf[TransformerInto[From, To, DisableDefaultValues[C]]]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`
    *
    * By default only Scala conversions (`.name`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/#ReadingfromJavabeans]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  def enableBeanGetters: TransformerInto[From, To, EnableBeanGetters[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableBeanGetters[C]]]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`
    *
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/#WritingtoJavabeans]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  def enableBeanSetters: TransformerInto[From, To, EnableBeanSetters[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableBeanSetters[C]]]

  /** Sets target value of optional field to None if field is missing from source type From
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Defaultoptionvalues]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  def enableOptionDefaultsToNone: TransformerInto[From, To, EnableOptionDefaultsToNone[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableOptionDefaultsToNone[C]]]

  /** Enable unsafe call to `.get` when source type From contains field of type `Option[A]`, but target type To defines this fields as `A`
    *
    * It's unsafe as code generated this way may throw at runtime.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Unsafeoption]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  def enableUnsafeOption: TransformerInto[From, To, EnableUnsafeOption[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableUnsafeOption[C]]]

  /** Enable values to be supplied from method calls. Source method must be public and have no parameter list.
    *
    * By default this is disabled because method calls may perform side effects (e.g. mutations)
    *
    * @see [[https://scalalandio.github.io/chimney/#UsingMethodCalls]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def enableMethodCalls: TransformerInto[From, To, EnableMethodCalls[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableMethodCalls[C]]]

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Providingmissingvalues]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  def withFieldConst[T, U](selector: To => T, value: U): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withFieldConstImpl[From, To, T, U, C]

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Providingmissingvalues]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function to use to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    * */
  def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Fieldre-labelling]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    * */
  def withFieldRenamed[T, U](selectorFrom: From => T, selectorTo: To => U): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withFieldRenamedImpl[From, To, T, U, C]

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts will have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it will fail.
    *
    * @see [[https://scalalandio.github.io/chimney/#Coproductssupport]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  def withCoproductInstance[Inst](f: Inst => To): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withCoproductInstanceImpl[From, To, Inst, C]

  /** Apply configured transformation in-place
    *
    * @return transformed value
    */
  def transform: To =
    macro ChimneyBlackboxMacros.transformImpl[From, To, C]
}
