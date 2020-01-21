package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.{ChimneyBlackboxMacros, TransformerIntoWhiteboxMacros}

import scala.language.experimental.macros

/** Provides DSL for configuring [[io.scalaland.chimney.Transformer]]'s
  * generation and using the result to transform value at the same time
  * 
  * For any ''.setting'' defined here (and in [[io.scalaland.chimney.dsl.TransformerDefinition]])
  * we can:
  * 
  * {{{
  * import io.scalaland.chimney.dsl._
  * foo.into[Bar] // <- creates this builder initializing it with foo as source and empty config
  *   .setting    // <- calls .setting on TransformerDefinition
  *   .transform  // <- calls .buildTransformer.transform(foo)
  * }}}
  * 
  * or (if only defaults are used, or there is some implicit [[io.scalaland.chimney.Transformer]]
  * already in scope):
  * 
  * {{{
  * import io.scalaland.chimney.dsl._
  * foo.transformInto[Bar]
  * }}}
  * 
  * @see [[io.scalaland.chimney.dsl.TransformerDefinition]] for building Transformer separately to applying it (in case you want to share it between several transformations)
  * 
  * @tparam From data type that will be used as input
  * @tparam To   data type that will be used as output
  * @tparam C    type-level encoded list of settings
  */
final class TransformerInto[From, To, C <: TransformerCfg](
    val source: From,
    val td: TransformerDefinition[From, To, C]
) {

  /** Fail derivation if `From` type is missing field even if `To` has default value for it
    * 
    * By default in such case derivation will fallback to default values.
    * 
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition#disableDefaultValues]]
    */
  def disableDefaultValues: TransformerInto[From, To, DisableDefaultValues[C]] =
    this.asInstanceOf[TransformerInto[From, To, DisableDefaultValues[C]]]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`
    * 
    * By default only Scala conversions (`.name`) are allowed.
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition#enableBeanGetters]]
    */
  def enableBeanGetters: TransformerInto[From, To, EnableBeanGetters[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableBeanGetters[C]]]

  /** Enable Java Beans naming convention (`.setName`) on `To`
    * 
    * By default only Scala conversions (`.name =`) are allowed.
    * 
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition#enableBeanSetters]]
    */
  def enableBeanSetters: TransformerInto[From, To, EnableBeanSetters[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableBeanSetters[C]]]

  /** Allow usage of None if field is missing in `From`, no rename/const value/calulated
    * value is available but present in `To` as `Option` type
    * 
    * By default in such case derivation will fail.
    * 
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition#enableOptionDefaultsToNone]]
    */
  def enableOptionDefaultsToNone: TransformerInto[From, To, EnableOptionDefaultsToNone[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableOptionDefaultsToNone[C]]]

  /** Allow running `.get` if `From` contains `Option[A]` and `To` requires `A`
    * 
    * By default in such case derivation will fail.
    * 
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition#enableUnsafeOption]]
    * */
  def enableUnsafeOption: TransformerInto[From, To, EnableUnsafeOption[C]] =
    this.asInstanceOf[TransformerInto[From, To, EnableUnsafeOption[C]]]

  /** Use `value` provided here for field picked using `selector`.
    * 
    * By default if `From` is missing field picked by `selector` derivation will fail.
    * 
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition#withFieldConst]] 
    * 
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    */
  def withFieldConst[T, U](selector: To => T, value: U): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withFieldConstImpl[From, To, T, U, C]

  /** Use `map` provided here to compute value of field picked using `selector`.
    * 
    * By default if `From` is missing field picked by `selector` derivation will fail.
    * 
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition#withFieldComputed]] 
    * 
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function to use to compute value of the target field
    * */
  def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    * 
    * By default if `From` is missing field picked by `selectorTo` derivation will fail.
    *
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition#withFieldRenamed]] 
    * 
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
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
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition#withCoproductInstance]]
    *
    * @param f function to calculate values of components that cannot be mapped automatically
    * 
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition#withCoproductInstance]]*/
  def withCoproductInstance[Inst](f: Inst => To): TransformerInto[From, To, _] =
    macro TransformerIntoWhiteboxMacros.withCoproductInstanceImpl[From, To, Inst, C]

  /** Derive [[io.scalaland.chimney.Transformer]] and pass transformed value into it */
  def transform: To =
    macro ChimneyBlackboxMacros.transformImpl[From, To, C]
}
