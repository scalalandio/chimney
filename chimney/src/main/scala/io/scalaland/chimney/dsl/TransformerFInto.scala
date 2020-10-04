package io.scalaland.chimney.dsl

import io.scalaland.chimney.TransformerFSupport
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.macros.{ChimneyBlackboxMacros, TransformerFIntoWhiteboxMacros}

import scala.language.experimental.macros

final class TransformerFInto[F[+_], From, To, C <: TransformerCfg](
    val source: From,
    val td: TransformerFDefinition[F, From, To, C]
) extends ConfigDsl[Lambda[`C1 <: TransformerCfg` => TransformerFInto[F, From, To, C1]], C] {

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def withFieldConst[T, U](selector: To => T, value: U): TransformerFInto[F, From, To, _ <: TransformerCfg] =
    macro TransformerFIntoWhiteboxMacros.withFieldConstImpl

  /** Use wrapped `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def withFieldConstF[T, U](selector: To => T, value: F[U]): TransformerFInto[F, From, To, _ <: TransformerCfg] =
    macro TransformerFIntoWhiteboxMacros.withFieldConstFImpl

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def withFieldComputed[T, U](
      selector: To => T,
      map: From => U
  ): TransformerFInto[F, From, To, _ <: TransformerCfg] =
    macro TransformerFIntoWhiteboxMacros.withFieldComputedImpl

  /** Use `map` provided here to compute wrapped value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def withFieldComputedF[T, U](
      selector: To => T,
      map: From => F[U]
  ): TransformerFInto[F, From, To, _ <: TransformerCfg] =
    macro TransformerFIntoWhiteboxMacros.withFieldComputedFImpl

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): TransformerFInto[F, From, To, _ <: TransformerCfg] =
    macro TransformerFIntoWhiteboxMacros.withFieldRenamedImpl

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def withCoproductInstance[Inst](f: Inst => To): TransformerInto[From, To, _ <: TransformerCfg] =
    macro TransformerFIntoWhiteboxMacros.withCoproductInstanceImpl

  /** Use `f` to calculate the (missing) wrapped coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  def withCoproductInstanceF[Inst](f: Inst => F[To]): TransformerFInto[F, From, To, _ <: TransformerCfg] =
    macro TransformerFIntoWhiteboxMacros.withCoproductInstanceFImpl

  /** Apply configured wrapped transformation in-place.
    *
    * It runs macro that tries to derive instance of `TransformerF[F, From, To]`
    * and immediately apply it to captured `source` value.
    *
    * It requires [[io.scalaland.chimney.TransformerFSupport]] instance for `F` to be
    * available in implicit scope of invocation of this method.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @return transformed value of type `F[To]`
    */
  def transform(implicit tfs: TransformerFSupport[F]): F[To] =
    macro ChimneyBlackboxMacros.transformFImpl[F, From, To, C]

  /** Used internally by macro. Please don't use in your code.
    */
  def __refineTransformerDefinition[C1 <: TransformerCfg](
      f: TransformerFDefinition[F, From, To, C] => TransformerFDefinition[F, From, To, C1]
  ): TransformerFInto[F, From, To, C1] =
    new TransformerFInto[F, From, To, C1](source, f(td))

}
