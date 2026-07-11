package io.scalaland.chimney

import io.scalaland.chimney.dsl.IsoDefinition
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

/** Type class representing bidirectional conversion between isomorphic types, where conversion in each can always
  * succeed (total transformation).
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#bidirectional-transformations]]
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
final case class Iso[First, Second](first: Transformer[First, Second], second: Transformer[Second, First]) {

  /** Creates a new [[io.scalaland.chimney.Iso]] with the `First` side replaced by `NewFirst`, given a bijection between
    * the two.
    *
    * @tparam NewFirst
    *   the new first type
    * @param f
    *   conversion from the old `First` to `NewFirst`
    * @param g
    *   conversion from `NewFirst` back to the old `First`
    * @return
    *   new [[io.scalaland.chimney.Iso]] between `NewFirst` and `Second`
    *
    * @since 2.0.0
    */
  def imapFirst[NewFirst](f: First => NewFirst)(g: NewFirst => First): Iso[NewFirst, Second] =
    Iso(first.contramap(g), second.map(f))

  /** Creates a new [[io.scalaland.chimney.Iso]] with the `Second` side replaced by `NewSecond`, given a bijection
    * between the two.
    *
    * @tparam NewSecond
    *   the new second type
    * @param f
    *   conversion from the old `Second` to `NewSecond`
    * @param g
    *   conversion from `NewSecond` back to the old `Second`
    * @return
    *   new [[io.scalaland.chimney.Iso]] between `First` and `NewSecond`
    *
    * @since 2.0.0
    */
  def imapSecond[NewSecond](f: Second => NewSecond)(g: NewSecond => Second): Iso[First, NewSecond] =
    Iso(first.map(f), second.contramap(g))
}

/** Companion of [[io.scalaland.chimney.Iso]].
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#bidirectional-transformations]]
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
