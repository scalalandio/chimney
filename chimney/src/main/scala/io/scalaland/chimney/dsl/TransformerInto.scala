package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.dsl.{TransformerBlackboxMacros, TransformerIntoWhiteboxMacros}

import scala.language.experimental.macros

/** Provides DSL for configuring [[io.scalaland.chimney.Transformer]]'s
  * generation and using the result to transform value at the same time
  *
  * @tparam From   type of input value
  * @tparam To     type of output value
  * @tparam C      type-level encoded config
  * @tparam Flags  type-level encoded flags
  * @param  source object to transform
  * @param  td     transformer definition
  *
  * @since 0.1.0
  */
final class TransformerInto[From, To, C <: TransformerCfg, Flags <: TransformerFlags](
    val source: From,
    val td: TransformerDefinition[From, To, C, Flags]
) extends FlagsDsl[Lambda[`F1 <: TransformerFlags` => TransformerInto[From, To, C, F1]], Flags] {

  /** Lifts current transformation with provided type constructor `F`.
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @tparam F    wrapper type constructor
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    *
    * @since 0.5.0
    */
  @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def lift[F[+_]]: TransformerFInto[F, From, To, WrapperType[F, C], Flags] =
    new TransformerFInto[F, From, To, WrapperType[F, C], Flags](source, td.lift[F])

  /** Lifts current transformation as partial transformation.
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    */
  def partial: PartialTransformerInto[From, To, C, Flags] =
    new PartialTransformerInto[From, To, C, Flags](source, td.partial)

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  def withFieldConst[T, U](selector: To => T, value: U)(
      implicit ev: U <:< T
  ): TransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withFieldConstImpl

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
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    *
    * @since 0.5.0
    */
  @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def withFieldConstF[F[+_], T, U](
      selector: To => T,
      value: F[U]
  )(implicit ev: U <:< T): TransformerFInto[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withFieldConstFImpl[F]

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
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): TransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withFieldComputedImpl

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
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    *
    * @since 0.5.0
    */
  @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def withFieldComputedF[F[+_], T, U](
      selector: To => T,
      f: From => F[U]
  )(implicit ev: U <:< T): TransformerFInto[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withFieldComputedFImpl[F]

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
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): TransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withFieldRenamedImpl

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts will have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it will fail.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    *
    * @tparam Inst type of coproduct instance@param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.2
    */
  def withCoproductInstance[Inst](f: Inst => To): TransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withCoproductInstanceImpl

  /** Use `f` to calculate the (missing) wrapped coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    *
    * @tparam Inst type of coproduct instance@param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    *
    * @since 0.5.0
    */
  @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def withCoproductInstanceF[F[+_], Inst](f: Inst => F[To]): TransformerFInto[F, From, To, _ <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withCoproductInstanceFImpl[F]

  /** Apply configured transformation in-place.
    *
    * It runs macro that tries to derive instance of `Transformer[From, To]`
    * and immediately apply it to captured `source` value.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return transformed value of type `To`
    *
    * @since 0.1.0
    */
  def transform[ScopeFlags <: TransformerFlags](
      implicit tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
  ): To =
    macro TransformerBlackboxMacros.transformImpl[From, To, C, Flags, ScopeFlags]

  /** Used internally by macro. Please don't use in your code.
    */
  def __refineTransformerDefinition[C1 <: TransformerCfg](
      f: TransformerDefinition[From, To, C, Flags] => TransformerDefinition[From, To, C1, Flags]
  ): TransformerInto[From, To, C1, Flags] =
    new TransformerInto[From, To, C1, Flags](source, f(td))
}
