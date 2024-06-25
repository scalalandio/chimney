package io.scalaland.chimney

import io.scalaland.chimney.dsl.IsoDefinition
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

/** Type class representing bidirectional conversion between isomorphic types, where conversion in each can always
  * succeed (total transformation).
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/TODO]]
  *
  * @tparam First
  *   input type of the first conversion, output type of the second conversion
  * @tparam Second
  *   output type of the first conversion, input type of the second conversion
  * @param first
  *   conversion the first type into the second type
  * @param second
  *   conversion the second type into the first type
  *
  * @since 1.2.0
  */
final case class Iso[First, Second](first: Transformer[First, Second], second: Transformer[Second, First])

/** Companion of [[io.scalaland.chimney.Iso]].
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/TODO]]
  *
  * @since 1.2.0
  */
object Iso extends IsoCompanionPlatform {

  /** Creates an empty [[io.scalaland.chimney.dsl.IsoDefinition]] that you can customize to derive
    * [[io.scalaland.chimney.Iso]].
    *
    * @see
    *   [[io.scalaland.chimney.dsl.IsoDefinition]] for available settings
    *
    * @tparam First
    *   input type of the first conversion, output type of the second conversion
    * @tparam Second
    *   output type of the first conversion, input type of the second conversion
    *
    * @return
    *   [[io.scalaland.chimney.dsl.IsoDefinition]] with defaults
    */
  def define[First, Second]
      : IsoDefinition[First, Second, TransformerOverrides.Empty, TransformerOverrides.Empty, TransformerFlags.Default] =
    new IsoDefinition(Transformer.define, Transformer.define)
}
