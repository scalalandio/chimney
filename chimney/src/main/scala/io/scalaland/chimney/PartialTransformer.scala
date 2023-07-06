package io.scalaland.chimney

import io.scalaland.chimney.dsl.{PartialTransformerDefinition, TransformerDefinitionCommons}
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags}

/** Type class expressing partial transformation between
  * source type `From` and target type `To`, with the ability
  * of reporting path-annotated transformation error(s).
  *
  * @tparam From type of input value
  * @tparam To   type of output value
  *
  * @since 0.7.0
  */
trait PartialTransformer[From, To] extends PartialTransformer.AutoDerived[From, To] {

  /** Run transformation using provided value as a source.
    *
    * @param src source value
    * @param failFast should fail as early as the first set of errors appear
    * @return result of transformation
    *
    * @since 0.7.0
    */
  def transform(src: From, failFast: Boolean): partial.Result[To]

  /** Run transformation using provided value as a source in error accumulation mode.
    *
    * @param src source value
    * @return result of transformation
    *
    * @since 0.7.0
    */
  final def transform(src: From): partial.Result[To] =
    transform(src, failFast = false)

  /** Run transformation using provided value as a source in short-circuit (fail fast) mode.
    *
    * @param src source value
    * @return result of transformation
    *
    * @since 0.7.0
    */
  final def transformFailFast(src: From): partial.Result[To] =
    transform(src, failFast = true)
}

object PartialTransformer extends PartialTransformerCompanionPlatform {

  /** Construct ad-hoc instance of partial transformer from transforming function returning partial result.
    *
    * @tparam From type of input value
    * @tparam To   type of output value
    * @param f transforming function returning partial result
    * @return [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  def apply[From, To](f: From => partial.Result[To]): PartialTransformer[From, To] =
    (src: From, _: Boolean) => {
      try {
        f(src)
      } catch {
        case why: Throwable => partial.Result.fromErrorThrowable(why)
      }
    }

  /** Construct ad-hoc instance of partial transformer from transforming function returning target value.
    *
    * @tparam From type of input value
    * @tparam To   type of output value
    * @param f transforming function returning target value
    * @return [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  def fromFunction[From, To](f: From => To): PartialTransformer[From, To] =
    (src: From, _: Boolean) => {
      try {
        partial.Result.fromValue(f(src))
      } catch {
        case why: Throwable => partial.Result.fromErrorThrowable(why)
      }
    }

  /** Lifts total transformer to partial transformer
    *
    * @tparam From type of input value
    * @tparam To   type of output value
    * @param t instance of total transformer
    * @return [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  def liftTotal[From, To](t: Transformer[From, To]): PartialTransformer[From, To] =
    fromFunction[From, To](t.transform)

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
  def define[From, To]: PartialTransformerDefinition[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new PartialTransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore)

  trait AutoDerived[From, To] {
    def transform(src: From, failFast: Boolean): partial.Result[To]
  }
  object AutoDerived extends PartialTransformerAutoDerivedCompanionPlatform {
    implicit def liftTotal[From, To](implicit total: Transformer[From, To]): AutoDerived[From, To] =
      (src: From, failFast: Boolean) => partial.Result.fromCatching(total.transform(src))
  }
}
