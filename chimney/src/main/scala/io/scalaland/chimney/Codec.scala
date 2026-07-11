package io.scalaland.chimney

import io.scalaland.chimney.dsl.CodecDefinition
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

/** Type class representing bidirectional conversion from the type that can be always converted (total transformation),
  * usually the domain model, into type that has to be validated when converting back (partial transformation), usually
  * the DTO model.
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#bidirectional-transformations]]
  *
  * @tparam Domain
  *   type of the domain value
  * @tparam Dto
  *   typeof the DTO value
  * @param encode
  *   conversion from domain model to DTO which is guaranteed to succeed
  * @param decode
  *   conversion from DTO to domain model which can fail
  *
  * @since 1.2.0
  */
final case class Codec[Domain, Dto](encode: Transformer[Domain, Dto], decode: PartialTransformer[Dto, Domain]) {

  /** Creates a new [[io.scalaland.chimney.Codec]] with the `Domain` side replaced by `NewDomain`, given a bijection
    * between the two.
    *
    * @tparam NewDomain
    *   the new domain type
    * @param f
    *   conversion from the old `Domain` to `NewDomain`
    * @param g
    *   conversion from `NewDomain` back to the old `Domain`
    * @return
    *   new [[io.scalaland.chimney.Codec]] between `NewDomain` and `Dto`
    *
    * @since 2.0.0
    */
  def imapDomain[NewDomain](f: Domain => NewDomain)(g: NewDomain => Domain): Codec[NewDomain, Dto] =
    Codec(encode.contramap(g), decode.map(f))

  /** Creates a new [[io.scalaland.chimney.Codec]] with the `Dto` side replaced by `NewDto`, given a bijection between
    * the two.
    *
    * @tparam NewDto
    *   the new DTO type
    * @param f
    *   conversion from the old `Dto` to `NewDto`
    * @param g
    *   conversion from `NewDto` back to the old `Dto`
    * @return
    *   new [[io.scalaland.chimney.Codec]] between `Domain` and `NewDto`
    *
    * @since 2.0.0
    */
  def imapDto[NewDto](f: Dto => NewDto)(g: NewDto => Dto): Codec[Domain, NewDto] =
    Codec(encode.map(f), decode.contramap(g))
}

/** Companion of [[io.scalaland.chimney.Codec]].
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#bidirectional-transformations]]
  *
  * @since 1.2.0
  */
object Codec extends CodecCompanionPlatform {

  /** Creates an empty [[io.scalaland.chimney.dsl.CodecDefinition]] that you can customize to derive
    * [[io.scalaland.chimney.Codec]].
    *
    * @see
    *   [[io.scalaland.chimney.dsl.CodecDefinition]] for available settings
    *
    * @tparam Domain
    *   type of the domain value
    * @tparam Dto
    *   typeof the DTO value
    *
    * @return
    *   [[io.scalaland.chimney.dsl.CodecDefinition]] with defaults
    */
  def define[Domain, Dto]
      : CodecDefinition[Domain, Dto, TransformerOverrides.Empty, TransformerOverrides.Empty, TransformerFlags.Default] =
    new CodecDefinition(Transformer.define, PartialTransformer.define)
}
