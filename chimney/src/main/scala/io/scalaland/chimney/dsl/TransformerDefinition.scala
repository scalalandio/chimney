package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.{ChimneyBlackboxMacros, TransformerDefinitionWhiteboxMacros}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.Transformer]] derivation
  * 
  * By default derivation will expect that:
  * 
  * - for every field in `To` there is a corresponding field in `From`
  * - their types match or it is possible to find/derive [[io.scalaland.chimney.Transformer]] for them
  * 
  * For some case this is insufficient and a manual intervention is needed:
  * 
  * - derivation should be informed that field in `From` corresponds with field
  *   of a different name in `To`,
  * - derivation should be informed that missing `To` field's value should be
  *   a constant/computed value,
  * - convertion to/from `Option` should get a special treatment,
  * - input/output type is a Java Bean with a convention different to Scala one's.
  * 
  * In such case you can use methods of this class to change derivation configuration.
  * 
  * {{{
  * implicit val fooToBar: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
  *   .setting1
  *   .setting2
  *   .buildTransformer
  * }}}
  * 
  * @see [[io.scalaland.chimney.dsl.TransformerInto]] for building transformer and applying it immediately
  * 
  * @tparam From data type that will be used as input
  * @tparam To   data type that will be used as output
  * @tparam C    type-level encoded config
  */
final class TransformerDefinition[From, To, C <: TransformerCfg](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) {

  /** Fail derivation if `From` type is missing field even if `To` has default value for it
    * 
    * By default in such case derivation will fallback to default values.
    */
  def disableDefaultValues: TransformerDefinition[From, To, DisableDefaultValues[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, DisableDefaultValues[C]]]

  /** Enable Java Beans naming convention (`.getName`, `.isName`) on `From`
    * 
    * By default only Scala conversions (`.name`) are allowed.
    */
  def enableBeanGetters: TransformerDefinition[From, To, EnableBeanGetters[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableBeanGetters[C]]]

  /** Enable Java Beans naming convention (`.setName(value)`) on `To`
    * 
    * By default only Scala conversions (`.copy(name = value)`) are allowed.
    */
  def enableBeanSetters: TransformerDefinition[From, To, EnableBeanSetters[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableBeanSetters[C]]]

  /** Allow usage of None if field is missing in `From`, no rename/const value/calulated
    * value is available but present in `To` as `Option` type
    * 
    * By default in such case derivation will fail.
    */
  def enableOptionDefaultsToNone: TransformerDefinition[From, To, EnableOptionDefaultsToNone[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableOptionDefaultsToNone[C]]]

  /** Allow running `.get` if `From` contains `Option[A]` and `To` requires `A`
    * 
    * By default in such case derivation will fail.
    */
  def enableUnsafeOption: TransformerDefinition[From, To, EnableUnsafeOption[C]] =
    this.asInstanceOf[TransformerDefinition[From, To, EnableUnsafeOption[C]]]

  /** Use `value` provided here for field picked using `selector`.
    * 
    * By default if `From` is missing field picked by `selector` derivation will fail.
    * 
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    */
  def withFieldConst[T, U](selector: To => T, value: U): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withFieldConstImpl[From, To, T, U, C]

  /** Use `map` provided here to compute value of field picked using `selector`.
    * 
    * By default if `From` is missing field picked by `selector` derivation will fail.
    * 
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function to use to compute value of the target field
    */
  def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    * 
    * By default if `From` is missing field picked by `selectorTo` derivation will fail.
    *
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
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
    * @param f function to calculate values of components that cannot be mapped automatically
    */
  def withCoproductInstance[Inst](f: Inst => To): TransformerDefinition[From, To, _] =
    macro TransformerDefinitionWhiteboxMacros.withCoproductInstanceImpl[From, To, Inst, C]

  /** Build Transformer using current configuration */
  def buildTransformer: Transformer[From, To] =
    macro ChimneyBlackboxMacros.buildTransformerImpl[From, To, C]
}
