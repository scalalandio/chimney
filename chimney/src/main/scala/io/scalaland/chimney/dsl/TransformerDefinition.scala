package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.dsl.{TransformerBlackboxMacros, TransformerDefinitionWhiteboxMacros}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.Transformer]] derivation.
  *
  * @tparam From  type of input value
  * @tparam To    type of output value
  * @tparam C     type-level encoded config
  * @tparam Flags type-level encoded flags
  *
  * @since 0.4.0
  */
final class TransformerDefinition[From, To, C <: TransformerCfg, Flags <: TransformerFlags](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) extends FlagsDsl[Lambda[`F1 <: TransformerFlags` => TransformerDefinition[From, To, C, F1]], Flags]
    with TransformerDefinitionCommons[Lambda[`C1 <: TransformerCfg` => TransformerDefinition[From, To, C1, Flags]]] {

  /** Lifts current transformer definition with provided type constructor `F`.
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @tparam F    wrapper type constructor
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    *
    * @since 0.5.0
    */
  @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def lift[F[+_]]: TransformerFDefinition[F, From, To, WrapperType[F, C], Flags] =
    new TransformerFDefinition[F, From, To, WrapperType[F, C], Flags](overrides, instances)

  /** Lifts current transformer definition as `PartialTransformer` definition
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    */
  def partial: PartialTransformerDefinition[From, To, C, Flags] =
    new PartialTransformerDefinition[From, To, C, Flags](overrides, instances)

  /** Use provided value `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of provided value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    * @since 0.4.0
    */
  def withFieldConst[T, U](selector: To => T, value: U)(
      implicit ev: U <:< T
  ): TransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro TransformerDefinitionWhiteboxMacros.withFieldConstImpl[C]

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
    * @since 0.5.0
    */
  @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def withFieldConstF[F[+_], T, U](
      selector: To => T,
      value: F[U]
  )(implicit ev: U <:< T): TransformerFDefinition[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerDefinitionWhiteboxMacros.withFieldConstFImpl[F]

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
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 0.4.0
    */
  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): TransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro TransformerDefinitionWhiteboxMacros.withFieldComputedImpl[C]

  /** Use function `f` to compute wrapped value of field picked using `selector`.
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
  @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def withFieldComputedF[F[+_], T, U](
      selector: To => T,
      f: From => F[U]
  )(implicit ev: U <:< T): TransformerFDefinition[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerDefinitionWhiteboxMacros.withFieldComputedFImpl[F]

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
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    *  @since 0.4.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): TransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro TransformerDefinitionWhiteboxMacros.withFieldRenamedImpl[C]

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
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 0.4.0
    */
  def withCoproductInstance[Inst](f: Inst => To): TransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro TransformerDefinitionWhiteboxMacros.withCoproductInstanceImpl[To, Inst, C]

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
  @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def withCoproductInstanceF[F[+_], Inst](
      f: Inst => F[To]
  ): TransformerFDefinition[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerDefinitionWhiteboxMacros.withCoproductInstanceFImpl[F]

  /** Build Transformer using current configuration.
    *
    * It runs macro that tries to derive instance of `Transformer[From, To]`.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[io.scalaland.chimney.Transformer]] type class instance
    *
    * @since 0.4.0
    */
  def buildTransformer[ScopeFlags <: TransformerFlags](
      implicit tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
  ): Transformer[From, To] =
    macro TransformerBlackboxMacros.buildTransformerImpl[From, To, C, Flags, ScopeFlags]

  override protected def updated(newOverrides: Map[String, Any], newInstances: Map[(String, String), Any]): this.type =
    new TransformerDefinition(newOverrides, newInstances).asInstanceOf[this.type]
}
