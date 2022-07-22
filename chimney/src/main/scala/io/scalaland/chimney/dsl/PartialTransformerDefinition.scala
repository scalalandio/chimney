package io.scalaland.chimney.dsl

import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.dsl.{PartialTransformerDefinitionWhiteboxMacros, TransformerBlackboxMacros}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.Transformer]] derivation
  *
  * @tparam From type of input value
  * @tparam To   type of output value
  * @tparam C    type-level encoded config
  */
final class PartialTransformerDefinition[From, To, C <: TransformerCfg, Flags <: TransformerFlags](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) extends FlagsDsl[Lambda[`F1 <: TransformerFlags` => PartialTransformerDefinition[From, To, C, F1]], Flags]
    with TransformerDefinitionCommons[
      Lambda[`C1 <: TransformerCfg` => PartialTransformerDefinition[From, To, C1, Flags]]
    ] {

  def withFieldConst[T, U](selector: To => T, value: U)(
      implicit ev: U <:< T
  ): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withFieldConstImpl[C]

  def withFieldConstPartial[T, U](selector: To => T, value: PartialTransformer.Result[U])(
      implicit ev: U <:< T
  ): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withFieldConstPartialImpl[C]

  def withFieldComputed[T, U](selector: To => T, map: From => U)(
      implicit ev: U <:< T
  ): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withFieldComputedImpl[C]

  def withFieldComputedPartial[T, U](
      selector: To => T,
      map: From => PartialTransformer.Result[U]
  )(implicit ev: U <:< T): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withFieldComputedPartialImpl[C]

  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withFieldRenamedImpl[C]

  def withCoproductInstance[Inst](f: Inst => To): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withCoproductInstanceImpl[To, Inst, C]

  def withCoproductInstancePartial[Inst](
      f: Inst => PartialTransformer.Result[To]
  ): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withCoproductInstancePartialImpl[To, Inst, C]

  def buildTransformer[ScopeFlags <: TransformerFlags](
      implicit tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
  ): PartialTransformer[From, To] =
    macro TransformerBlackboxMacros.buildPartialTransformerImpl[From, To, C, Flags, ScopeFlags]

  override protected def updated(newOverrides: Map[String, Any], newInstances: Map[(String, String), Any]): this.type =
    new PartialTransformerDefinition(newOverrides, newInstances).asInstanceOf[this.type]
}
