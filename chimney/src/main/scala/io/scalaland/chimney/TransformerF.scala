package io.scalaland.chimney

import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros

/** Maps data from one type `From` into another `F[To]`.
  *
  * @tparam F    type of context output lifted to
  * @tparam From type of input value
  * @tparam To   type of output value
  */
trait TransformerF[F[_], From, To] {
  def transform(src: From): F[To]
}

object TransformerF {

  /** Provides [[io.scalaland.chimney.TransformerF]] derived with the default settings.
    *
    * @tparam F    type of context output lifted to
    * @tparam From type of input value
    * @tparam To   type of output value
    * @return [[io.scalaland.chimney.TransformerF]] type class definition
    */
  implicit def derive[F[_], From, To]: TransformerF[F, From, To] =
    macro ChimneyBlackboxMacros.deriveTransformerImpl[F, From, To]

  /** Creates an empty [[io.scalaland.chimney.dsl.TransformerDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.TransformerF]].
    *
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition]] for available settings

    * @tparam F    type of context output lifted to
    * @tparam From type of input value
    * @tparam To   type of output value
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]] with defaults
    */
  def define[F[_]: TransformationContext, From, To]: TransformerDefinition[F, From, To, TransformerCfg.Empty] =
    new TransformerDefinition[F, From, To, TransformerCfg.Empty](Map.empty, Map.empty)
}
