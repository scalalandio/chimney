package io.scalaland.chimney

import io.scalaland.chimney.dsl.{PartialTransformerDefinition, TransformerDefinitionCommons}
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

/** Type class expressing partial transformation between source type `From` and target type `To`, with the ability of
  * reporting path-annotated transformation error(s).
  *
  * @note
  *   You should not need to instantiate this class manually, if you can derive it - take a look at
  *   [[io.scalaland.chimney.PartialTransformer.derive]] and [[io.scalaland.chimney.PartialTransformer.define]] methods
  *   for that. Manual intantiation is only necessary if you want to add support for the transformation that is not
  *   supported out of the box. Even then consult [[https://chimney.readthedocs.io/cookbook/#integrations]] first!
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
  * @since 0.7.0
  */
@FunctionalInterface
trait PartialTransformer[From, To] {

  /** Run transformation using provided value as a source.
    *
    * @param src
    *   source value
    * @param failFast
    *   whether the transformation should return as early as the first set of errors appear (`true`), or should it
    *   attempt to convert what it can and then aggregate all errors (`false`)
    * @return
    *   [[io.scalaland.chimney.partial.Result]] of the transformation
    *
    * @since 0.7.0
    */
  def transform(src: From, failFast: Boolean): partial.Result[To]

  /** Run transformation using provided value as a source in error accumulation mode.
    *
    * @param src
    *   source value
    * @return
    *   [[io.scalaland.chimney.partial.Result]] of the transformation
    *
    * @since 0.7.0
    */
  final def transform(src: From): partial.Result[To] =
    transform(src, failFast = false)

  /** Run transformation using provided value as a source in short-circuit (fail fast) mode.
    *
    * @param src
    *   source value
    * @return
    *   [[io.scalaland.chimney.partial.Result]] of the transformation
    *
    * @since 0.7.0
    */
  final def transformFailFast(src: From): partial.Result[To] =
    transform(src, failFast = true)
}

/** Companion of [[io.scalaland.chimney.PartialTransformer]].
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/]]
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#total-transformers-vs-partialtransformers]]
  *
  * @since 0.7.0
  */
object PartialTransformer extends PartialTransformerLowPriorityImplicits1 {

  /** Construct ad-hoc instance of partial transformer from transforming function returning partial result.
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @param f
    *   transforming function returning partial result
    * @return
    *   [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  def apply[From, To](f: From => partial.Result[To]): PartialTransformer[From, To] =
    (src: From, _: Boolean) =>
      try
        f(src)
      catch {
        case why: Throwable => partial.Result.fromErrorThrowable(why)
      }

  /** Construct ad-hoc instance of partial transformer from transforming function returning target value.
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @param f
    *   transforming function returning target value
    * @return
    *   [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  def fromFunction[From, To](f: From => To): PartialTransformer[From, To] =
    (src: From, _: Boolean) =>
      try
        partial.Result.fromValue(f(src))
      catch {
        case why: Throwable => partial.Result.fromErrorThrowable(why)
      }

  /** Lifts total transformer to partial transformer
    *
    * @tparam From
    *   type of input value
    * @tparam To
    *   type of output value
    * @param t
    *   instance of total transformer
    * @return
    *   [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  def liftTotal[From, To](t: Transformer[From, To]): PartialTransformer[From, To] =
    fromFunction[From, To](t.transform)

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
  def define[From, To]: PartialTransformerDefinition[From, To, TransformerOverrides.Empty, TransformerFlags.Default] =
    new PartialTransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore)

  /** Type class used when you want to allow using automatically derived transformations.
    *
    * When we want to only allow semiautomatically derived/manually defined instances you should use
    * [[io.scalaland.chimney.PartialTransformer]].
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
  type AutoDerived[From, To] = Transformer[From, To]
}
private[chimney] trait PartialTransformerLowPriorityImplicits1 extends PartialTransformerCompanionPlatform {
  this: PartialTransformer.type =>

  /** Extracts [[io.scalaland.chimney.PartialTransformer]] from existing [[io.scalaland.chimney.Codec.decode]].
    *
    * @tparam Domain
    *   type of domain value
    * @tparam Dto
    *   type of DTO value
    *
    * @since 1.2.0
    */
  implicit def partialTransformerFromCodecDecoder[Dto, Domain](implicit
      codec: Codec[Domain, Dto]
  ): PartialTransformer[Dto, Domain] =
    codec.decode
}
