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
class TransformerDefinition[From, To, C <: TransformerCfg](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) extends ConfigDsl[Lambda[`C1 <: TransformerCfg` => TransformerDefinition[From, To, C1]], C] {

  /** Lifts current transformer definition with provided type constructor `F`.
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @tparam F    wrapper type constructor
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  def lift[F[+_]]: TransformerFDefinition[F, From, To, WrapperType[F, C]] =
    new TransformerFDefinition[F, From, To, WrapperType[F, C]](overrides, instances)

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def withFieldConst[T, U](selector: To => T, value: U): TransformerDefinition[From, To, _ <: TransformerCfg] =
    macro TransformerDefinitionWhiteboxMacros.withFieldConstImpl[From, To, T, U, C]

  /** Use wrapped `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  def withFieldConstF[F[+_], T, U](
      selector: To => T,
      value: F[U]
  ): TransformerFDefinition[F, From, To, _ <: TransformerCfg] =
    macro TransformerDefinitionWhiteboxMacros.withFieldConstFImpl[F]

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerDefinition[From, To, _ <: TransformerCfg] =
    macro TransformerDefinitionWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C]

  /** Use `map` provided here to compute wrapped value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  def withFieldComputedF[F[+_], T, U](
      selector: To => T,
      map: From => F[U]
  ): TransformerFDefinition[F, From, To, _ <: TransformerCfg] =
    macro TransformerDefinitionWhiteboxMacros.withFieldComputedFImpl[F]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): TransformerDefinition[From, To, _ <: TransformerCfg] =
    macro TransformerDefinitionWhiteboxMacros.withFieldRenamedImpl[From, To, T, U, C]

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    */
  def withCoproductInstance[Inst](f: Inst => To): TransformerDefinition[From, To, _ <: TransformerCfg] =
    macro TransformerDefinitionWhiteboxMacros.withCoproductInstanceImpl[From, To, Inst, C]

  /** Use `f` to calculate the (missing) wrapped coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  def withCoproductInstanceF[F[+_], Inst](f: Inst => F[To]): TransformerFDefinition[F, From, To, _ <: TransformerCfg] =
    macro TransformerDefinitionWhiteboxMacros.withCoproductInstanceFImpl[F, From, To, Inst, C]

  /** Build Transformer using current configuration.
    *
    * It runs macro that tries to derive instance of `Transformer[From, To]`.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[io.scalaland.chimney.Transformer]] type class instance
    */
  def buildTransformer: Transformer[From, To] =
    macro ChimneyBlackboxMacros.buildTransformerImpl[From, To, C]

  /** Used internally by macro. Please don't use in your code.
    */
  def __refineConfig[C1 <: TransformerCfg]: TransformerDefinition[From, To, C1] =
    this.asInstanceOf[TransformerDefinition[From, To, C1]]

  /** Used internally by macro. Please don't use in your code.
    */
  def __addOverride(key: String, value: Any): TransformerDefinition[From, To, C] =
    new TransformerDefinition(overrides.updated(key, value), instances)

  /** Used internally by macro. Please don't use in your code.
    */
  def __addInstance(from: String, to: String, value: Any): TransformerDefinition[From, To, C] =
    new TransformerDefinition(overrides, instances.updated((from, to), value))

}
