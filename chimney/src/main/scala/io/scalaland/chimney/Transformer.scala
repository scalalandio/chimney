package io.scalaland.chimney

import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}
import io.scalaland.chimney.dsl.{PartialTransformerDefinition, TransformerDefinition, TransformerDefinitionCommons, TransformerFDefinition}
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros

import scala.language.experimental.macros

/** Type class expressing total transformation between
  * source type `From` and target type `To`.
  *
  * @tparam From type of input value
  * @tparam To   type of output value
  *
  * @since 0.1.0
  */
trait Transformer[From, To] {

  /** Run transformation using provided value as a source.
    *
    * @param src source value
    * @return     transformed value
    *
    * @since 0.1.0
    */
  def transform(src: From): To
}

object Transformer {

  /** Provides [[io.scalaland.chimney.Transformer]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.Transformer]] type class instance
    *
    * @since 0.2.0
    */
  implicit def derive[From, To]: Transformer[From, To] =
    macro TransformerBlackboxMacros.deriveTransformerImpl[From, To]

  /** Creates an empty [[io.scalaland.chimney.dsl.TransformerDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.Transformer]].
    *
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition]] for available settings
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]] with defaults
    *
    * @since 0.4.0
    */
  def define[From, To]: TransformerDefinition[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new TransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore)

  /** Creates an empty [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.PartialTransformer]].
    *
    * @see [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] for available settings
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] with defaults
    *
    * @since 0.7.0
    */
  def definePartial[From, To]: PartialTransformerDefinition[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new PartialTransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore)

  /** Creates an empty [[io.scalaland.chimney.dsl.TransformerFDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.TransformerF]].
    *
    * @see [[io.scalaland.chimney.dsl.TransformerFDefinition]] for available settings
    *
    * @tparam F    wrapper type constructor
    * @tparam From type of input value
    * @tparam To   type of output value
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]] with defaults
    *
    * @since 0.5.0
    */
  @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
  def defineF[F[+_], From, To]
      : TransformerFDefinition[F, From, To, TransformerCfg.WrapperType[F, TransformerCfg.Empty], TransformerFlags.Default] =
    TransformerF.define[F, From, To]
}
