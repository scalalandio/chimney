package io.scalaland.chimney

import io.scalaland.chimney.dsl.{PartialTransformerDefinition, TransformerDefinition, TransformerDefinitionCommons}
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

/** Type class expressing total transformation between source type `From` and target type `To`.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/]]
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#total-transformers-vs-partialtransformers]]
  *
  * @tparam From
  *   type of input value
  * @tparam To
  *   type of output value
  *
  * @since 0.1.0
  */
@FunctionalInterface
trait Transformer[From, To] extends Transformer.AutoDerived[From, To] { self =>

  /** Run transformation using provided value as a source.
    *
    * @param src
    *   source value
    * @return
    *   transformed value
    *
    * @since 0.1.0
    */
  def transform(src: From): To

  /** Creates a new [[io.scalaland.chimney.Transformer Transformer]] by applying a pure function to a source of type `A`
    * before transforming it to `To`. See an example:
    * {{{
    *   val stringTransformer: Transformer[String, Int] = _.length
    *
    *   case class Id(id: String)
    *
    *   implicit val idTransformer: Transformer[Id, Int] =
    *     stringTransformer.contramap(_.id)
    * }}}
    *
    * @param f
    *   a pure function that maps a value of `A` to `From`
    * @return
    *   new [[io.scalaland.chimney.Transformer Transformer]]
    *
    * @since 1.5.0
    */
  final def contramap[A](f: A => From): Transformer[A, To] = new Transformer[A, To] {
    override def transform(src: A): To = self.transform(f(src))
  }

  /** Creates a new [[io.scalaland.chimney.Transformer Transformer]] by applying a pure function to a result of
    * transforming `From` to `To`. See an example:
    * {{{
    *   val stringTransformer: Transformer[String, Int] = _.length
    *
    *   case class Length(length: Int)
    *
    *   implicit val toLengthTransformer: Transformer[String, Length] =
    *     stringTransformer.map(id => Length(id))
    * }}}
    *
    * @param f
    *   a pure function that maps a value of `To` to `A`
    * @return
    *   new [[io.scalaland.chimney.Transformer Transformer]]
    *
    * @since 1.5.0
    */
  final def map[A](f: To => A): Transformer[From, A] = new Transformer[From, A] {
    override def transform(src: From): A = f(self.transform(src))
  }
}

/** Companion of [[io.scalaland.chimney.Transformer]].
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/]]
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#total-transformers-vs-partialtransformers]]
  *
  * @since 0.2.0
  */
object Transformer extends TransformerCompanionPlatform {

  /** Creates an empty [[io.scalaland.chimney.dsl.TransformerDefinition]] that you can customize to derive
    * [[io.scalaland.chimney.Transformer]].
    *
    * @see
    *   [[io.scalaland.chimney.dsl.TransformerDefinition]] for available settings
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerDefinition]] with defaults
    *
    * @since 0.4.0
    */
  def define[From, To]: TransformerDefinition[From, To, TransformerOverrides.Empty, TransformerFlags.Default] =
    new TransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore)

  /** Creates an empty [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] that you can customize to derive
    * [[io.scalaland.chimney.PartialTransformer]].
    *
    * @see
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] for available settings
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] with defaults
    *
    * @since 0.7.0
    */
  def definePartial[From, To]
      : PartialTransformerDefinition[From, To, TransformerOverrides.Empty, TransformerFlags.Default] =
    new PartialTransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore)

  /** Type class used when you want o allow using automatically derived transformations.
    *
    * When we want to only allow semiautomatically derived/manually defined instances you should use
    * [[io.scalaland.chimney.Transformer]].
    *
    * @see
    *   [[https://chimney.readthedocs.io/cookbook/#automatic-semiautomatic-and-inlined-derivation]] for more details
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    *
    * @since 0.8.0
    */
  @FunctionalInterface
  trait AutoDerived[From, To] {
    def transform(src: From): To
  }

  /** @since 0.8.0 */
  object AutoDerived extends TransformerAutoDerivedCompanionPlatform
}
// extended by TransformerCompanionPlatform
private[chimney] trait TransformerLowPriorityImplicits1 extends TransformerLowPriorityImplicits2 {
  this: Transformer.type =>

  /** Extracts [[io.scalaland.chimney.Transformer]] from existing [[io.scalaland.chimney.Iso#left]].
    *
    * @tparam First
    *   input type of the first conversion, output type of the second conversion
    * @tparam Second
    *   output type of the first conversion, input type of the second conversion
    *
    * @since 1.2.0
    */
  implicit def transformerFromIsoFirst[First, Second](implicit iso: Iso[First, Second]): Transformer[First, Second] =
    iso.first
}
private[chimney] trait TransformerLowPriorityImplicits2 extends TransformerLowPriorityImplicits3 {
  this: Transformer.type =>

  /** Extracts [[io.scalaland.chimney.Transformer]] from existing [[io.scalaland.chimney.Iso#right]].
    *
    * @tparam First
    *   input type of the first conversion, output type of the second conversion
    * @tparam Second
    *   output type of the first conversion, input type of the second conversion
    *
    * @since 1.2.0
    */
  implicit def transformerFromIsoSecond[First, Second](implicit iso: Iso[First, Second]): Transformer[Second, First] =
    iso.second
}
private[chimney] trait TransformerLowPriorityImplicits3 { this: Transformer.type =>

  /** Extracts [[io.scalaland.chimney.Transformer]] from existing [[io.scalaland.chimney.Codec#encode]].
    *
    * @tparam Domain
    *   type of domain value
    * @tparam Dto
    *   type of DTO value
    *
    * @since 1.2.0
    */
  implicit def transformerFromCodecEncoder[Domain, Dto](implicit codec: Codec[Domain, Dto]): Transformer[Domain, Dto] =
    codec.encode
}
