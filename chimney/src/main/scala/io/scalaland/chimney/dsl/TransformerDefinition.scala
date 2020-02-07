package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.{ChimneyBlackboxMacros, TransformerDefinitionWhiteboxMacros}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.Transformer]] derivation
  *
  * @tparam From type of input value
  * @tparam To   type of output value
  * @tparam C    type-level encoded config
  */
final class TransformerDefinition[From, To, C <: TransformerCfg](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) {

  /** Fail derivation if `From` type is missing field even if `To` has default value for it
    *
    * By default in such case derivation will fallback to default values.
    *
    * @see [[https://scalalandio.github.io/chimney/#Defaultoptionvalues]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def disableDefaultValues: TransformerDefinition[From, To, DisableDefaultValues[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, DisableDefaultValues[C]]]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`
    *
    * By default only Scala conversions (`.name`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/#ReadingfromJavabeans]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def enableBeanGetters: TransformerDefinition[From, To, EnableBeanGetters[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableBeanGetters[C]]]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`
    *
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    *
    * @see [[https://scalalandio.github.io/chimney/#WritingtoJavabeans]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def enableBeanSetters: TransformerDefinition[From, To, EnableBeanSetters[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableBeanSetters[C]]]

  /** Sets target value of optional field to None if field is missing from source type From
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Defaultoptionvalues]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def enableOptionDefaultsToNone: TransformerDefinition[From, To, EnableOptionDefaultsToNone[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableOptionDefaultsToNone[C]]]

  /** Enable unsafe call to `.get` when source type From contains field of type `Option[A]`, but target type To defines this fields as `A`
    *
    * It's unsafe as code generated this way may throw at runtime.
    *
    * By default in such case compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Unsafeoption]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def enableUnsafeOption: TransformerDefinition[From, To, EnableUnsafeOption[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableUnsafeOption[C]]]

  /** Enable values to be supplied from method calls. Source method must be public and have no parameter list.
    *
    * By default this is disabled because method calls may perform side effects (e.g. mutations)
    *
    * @see [[https://scalalandio.github.io/chimney/#UsingMethodCalls]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def enableMethodCalls: TransformerDefinition[From, To, EnableMethodCalls[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableMethodCalls[C]]]

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Providingmissingvalues]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def withFieldConst[T, U](selector: To => T, value: U): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withFieldConstImpl[From, To, T, U, C]

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Providingmissingvalues]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function to use to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/#Fieldre-labelling]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def withFieldRenamed[T, U](selectorFrom: From => T, selectorTo: To => U): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withFieldRenamedImpl[From, To, T, U, C]

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts will have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it will fail.
    *
    * @see [[https://scalalandio.github.io/chimney/#Coproductssupport]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def withCoproductInstance[Inst](f: Inst => To): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withCoproductInstanceImpl[From, To, Inst, C]

  /** Build Transformer using current configuration
    *
    * @return [[io.scalaland.chimney.Transformer]] type class definition
    */
  def buildTransformer: Transformer[From, To] =
    macro ChimneyBlackboxMacros.buildTransformerImpl[From, To, C]
}
