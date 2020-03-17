package io.scalaland.chimney.dsl

import io.scalaland.chimney.{TransformerF, TransformerFSupport}
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.{ChimneyBlackboxMacros, TransformerFDefinitionWhiteboxMacros}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.TransformerF]] derivation
  *
  * @tparam F    wrapper type constructor
  * @tparam From type of input value
  * @tparam To   type of output value
  * @tparam C    type-level encoded config
  */
final class TransformerFDefinition[F[+_], From, To, C <: TransformerCfg](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) extends ConfigDsl[Lambda[`C1 <: TransformerCfg` => TransformerFDefinition[F, From, To, C1]], C] {

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  def withFieldConst[T, U](selector: To => T, value: U): TransformerFDefinition[F, From, To, _ <: TransformerCfg] =
    macro TransformerFDefinitionWhiteboxMacros.withFieldConstImpl[From, To, T, U, C]

  /** Use wrapped `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  def withFieldConstF[T, U](selector: To => T, value: F[U]): TransformerFDefinition[F, From, To, _ <: TransformerCfg] =
    macro TransformerFDefinitionWhiteboxMacros.withFieldConstFImpl[From, To, T, U, C, F]

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  def withFieldComputed[T, U](
      selector: To => T,
      map: From => U
  ): TransformerFDefinition[F, From, To, _ <: TransformerCfg] =
    macro TransformerFDefinitionWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C]

  /** Use `map` provided here to compute wrapped value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  def withFieldComputedF[T, U](
      selector: To => T,
      map: From => F[U]
  ): TransformerFDefinition[F, From, To, _ <: TransformerCfg] =
    macro TransformerFDefinitionWhiteboxMacros.withFieldComputedFImpl[From, To, T, U, C, F]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): TransformerFDefinition[F, From, To, _ <: TransformerCfg] =
    macro TransformerFDefinitionWhiteboxMacros.withFieldRenamedImpl[From, To, T, U, C]

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
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
  def withCoproductInstance[Inst <: From](f: Inst => To): TransformerFDefinition[F, From, To, _ <: TransformerCfg] =
    macro TransformerFDefinitionWhiteboxMacros.withCoproductInstanceImpl[From, To, Inst, C]

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
  def withCoproductInstanceF[Inst](f: Inst => F[To]): TransformerFDefinition[F, From, To, _ <: TransformerCfg] =
    macro TransformerFDefinitionWhiteboxMacros.withCoproductInstanceFImpl[From, To, Inst, C]

  /** Build TransformerF using current configuration.
    *
    * It runs macro that tries to derive instance of `TransformerF[F, From, To]`.
    * It requires [[io.scalaland.chimney.TransformerFSupport[F]]] instance to be
    * available in implicit scope of invocation of this method.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[io.scalaland.chimney.TransformerF]] type class instance
    */
  def buildTransformer(implicit tfs: TransformerFSupport[F]): TransformerF[F, From, To] =
    macro ChimneyBlackboxMacros.buildTransformerFImpl[F, From, To, C]

  /** Used internally by macro. Please don't use in your code.
    */
  def __refineConfig[C1 <: TransformerCfg]: TransformerFDefinition[F, From, To, C1] =
    this.asInstanceOf[TransformerFDefinition[F, From, To, C1]]

  /** Used internally by macro. Please don't use in your code.
    */
  def __addOverride(key: String, value: Any): TransformerFDefinition[F, From, To, C] =
    new TransformerFDefinition(overrides.updated(key, value), instances)

  /** Used internally by macro. Please don't use in your code.
    */
  def __addInstance(from: String, to: String, value: Any): TransformerFDefinition[F, From, To, C] =
    new TransformerFDefinition(overrides, instances.updated((from, to), value))
}
