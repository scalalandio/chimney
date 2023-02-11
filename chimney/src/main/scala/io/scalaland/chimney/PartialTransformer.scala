package io.scalaland.chimney

import io.scalaland.chimney.dsl.PartialTransformerDefinition
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros
import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}

import scala.language.experimental.macros

/** Type class expressing partial transformation between
  * source type `From` and target type `To`, with the ability
  * of reporting transformation error
  *
  * @tparam From type of input value
  * @tparam To   type of output value
  */
trait PartialTransformer[From, To] {

  def transform(src: From, failFast: Boolean): partial.Result[To]

  final def transform(src: From): partial.Result[To] =
    transform(src, failFast = false)

  final def transformFailFast(src: From): partial.Result[To] =
    transform(src, failFast = true)
}

object PartialTransformer {

  def apply[A, B](f: A => partial.Result[B]): PartialTransformer[A, B] =
    (src: A, _: Boolean) => f(src)

  /** Provides [[io.scalaland.chimney.PartialTransformer]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.PartialTransformer]] type class definition
    */
  implicit def derive[From, To]: PartialTransformer[From, To] =
    macro TransformerBlackboxMacros.derivePartialTransformerImpl[From, To]

  /** Creates an empty [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.PartialTransformer]].
    *
    * @see [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] for available settings
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] with defaults
    */
  def define[From, To]: PartialTransformerDefinition[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new PartialTransformerDefinition(Map.empty, Map.empty)

}
