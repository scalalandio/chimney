package io.scalaland.chimney.dsl

import io.scalaland.chimney.{TransformerF, TransformerFSupport}
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.dsl.{TransformerBlackboxMacros, TransformerFDefinitionWhiteboxMacros}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.TransformerF]] derivation
  *
  * @tparam F     wrapper type constructor
  * @tparam From  type of input value
  * @tparam To    type of output value
  * @tparam C     type-level encoded config
  * @tparam Flags type-level encoded flags
  *
  * @since 0.5.0
  */
@deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "0.7.0")
final class TransformerFDefinition[F[+_], From, To, C <: TransformerCfg, Flags <: TransformerFlags](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) extends FlagsDsl[Lambda[`F1 <: TransformerFlags` => TransformerFDefinition[F, From, To, C, F1]], Flags]
    with TransformerDefinitionCommons[Lambda[`C1 <: TransformerCfg` => TransformerFDefinition[F, From, To, C1, Flags]]] {

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of provided value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    * @since 0.5.0
    */
  def withFieldConst[T, U](
      selector: To => T,
      value: U
  )(implicit ev: U <:< T): TransformerFDefinition[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerFDefinitionWhiteboxMacros.withFieldConstImpl[C]

  /** Use wrapped `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of provided value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    *
    *  @since 0.5.0
    */
  def withFieldConstF[T, U](
      selector: To => T,
      value: F[U]
  )(implicit ev: U <:< T): TransformerFDefinition[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerFDefinitionWhiteboxMacros.withFieldConstFImpl[C, F]

  /** Use function `f` to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    *
    * @since 0.5.0
    */
  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): TransformerFDefinition[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerFDefinitionWhiteboxMacros.withFieldComputedImpl[C]

  /** Use `f` provided here to compute wrapped value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    *
    * @since 0.5.0
    */
  def withFieldComputedF[T, U](
      selector: To => T,
      f: From => F[U]
  )(implicit ev: U <:< T): TransformerFDefinition[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerFDefinitionWhiteboxMacros.withFieldComputedFImpl[C, F]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    *
    * @tparam T type of source field
    * @tparam U type of target field
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    *
    *  @since 0.5.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): TransformerFDefinition[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerFDefinitionWhiteboxMacros.withFieldRenamedImpl[C]

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    *
    * @tparam Inst type of coproduct instance
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    *
    *  @since 0.5.0
    */
  def withCoproductInstance[Inst <: From](
      f: Inst => To
  ): TransformerFDefinition[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerFDefinitionWhiteboxMacros.withCoproductInstanceImpl[To, Inst, C]

  /** Use `f` to calculate the (missing) wrapped coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    *
    * @tparam Inst type of coproduct instance
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    *
    * @since 0.5.0
    */
  def withCoproductInstanceF[Inst](f: Inst => F[To]): TransformerFDefinition[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerFDefinitionWhiteboxMacros.withCoproductInstanceFImpl[To, Inst, C]

  /** Build TransformerF using current configuration.
    *
    * It runs macro that tries to derive instance of `TransformerF[F, From, To]`.
    *
    * It requires [[io.scalaland.chimney.TransformerFSupport]] instance for `F` to be
    * available in implicit scope of invocation of this method.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[io.scalaland.chimney.TransformerF]] type class instance
    *
    * @since 0.5.0
    */
  def buildTransformer[ScopeFlags <: TransformerFlags](
      implicit tfs: TransformerFSupport[F],
      tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
  ): TransformerF[F, From, To] =
    macro TransformerBlackboxMacros.buildTransformerFImpl[F, From, To, C, Flags, ScopeFlags]

  override protected def updated(newOverrides: Map[String, Any], newInstances: Map[(String, String), Any]): this.type =
    new TransformerFDefinition(newOverrides, newInstances).asInstanceOf[this.type]
}
