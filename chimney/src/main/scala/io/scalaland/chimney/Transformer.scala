package io.scalaland.chimney

import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros

/** Maps data from one type `From` into another `To`.
  *
  * @tparam From data type that will be used as input
  * @tparam To   data type that will be used as output
  */
trait Transformer[From, To] {
  def transform(src: From): To
}

object Transformer {

  /** Provides [[io.scalaland.chimney.Transformer]] derived with the default settings.
    *
    * @tparam From data type that will be used as input
    * @tparam Into data type that will be used as output
    * @return [[io.scalaland.chimney.Transformer]] type class definition
    */
  implicit def derive[From, To]: Transformer[From, To] =
    macro ChimneyBlackboxMacros.deriveTransformerImpl[From, To]

  /** Creates an empty [[io.scalaland.chimney.dsl.TransformerDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.Transformer]].
    *
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition]] for available settings
    *
    * @tparam From data type that will be used as input
    * @tparam Into data type that will be used as output
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]] with defaults
    */
  def define[From, To]: TransformerDefinition[From, To, TransformerCfg.Empty] =
    new TransformerDefinition[From, To, TransformerCfg.Empty](Map.empty, Map.empty)
}
