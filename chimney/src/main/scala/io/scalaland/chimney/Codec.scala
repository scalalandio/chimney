package io.scalaland.chimney

import io.scalaland.chimney.dsl.CodecDefinition
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

/** Type class representing bidirectional conversion from the type that can be always converted (total transformation),
  * usually the domain model, into type that has to be validated when converting back (partial transformation), usually
  * the DTO model.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/TODO]]
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
final case class Codec[Domain, Dto](encode: Transformer[Domain, Dto], decode: PartialTransformer[Dto, Domain])
    extends Codec.AutoDerived[Domain, Dto]

/** Companion of [[io.scalaland.chimney.Codec]].
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/TODO]]
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

  /** Type class used when you want o allow using automatically derived transformations.
    *
    * When we want to only allow semiautomatically derived/manually defined instances you should use
    * [[io.scalaland.chimney.Codec]].
    *
    * @see
    *   [[https://chimney.readthedocs.io/cookbook/#automatic-semiautomatic-and-inlined-derivation]] for more details
    *
    * @tparam Domain
    *   type of the domain value
    * @tparam Dto
    *   typeof the DTO value
    *
    * @since 1.2.0
    */
  trait AutoDerived[Domain, Dto] {
    val encode: Transformer[Domain, Dto]
    val decode: PartialTransformer[Dto, Domain]
  }

  /** @since 1.2.0 */
  object AutoDerived extends CodecAutoDerivedCompanionPlatform
}
