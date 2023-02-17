package io.scalaland.chimney

import io.scalaland.chimney.dsl.PartialTransformerDefinition
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros
import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}

import scala.language.experimental.macros

/** Type class expressing partial transformation between
  * source type `From` and target type `To`, with the ability
  * of reporting path-annotated transformation error(s).
  *
  * @tparam From type of input value
  * @tparam To   type of output value
  *
  * @since 0.7.0
  */
trait PartialTransformer[From, To] {

  /** Run transformation using provided value as a source.
    *
    * @param src source value
    * @param failFast should fail as early as the first set of errors appear
    *
    * @since 0.7.0
    */
  def transform(src: From, failFast: Boolean): partial.Result[To]

  /** Run transformation using provided value as a source in error accumulation mode.
    *
    * @param src source value
    *
    * @since 0.7.0
    */
  final def transform(src: From): partial.Result[To] =
    transform(src, failFast = false)

  /** Run transformation using provided value as a source in short-circuit (fail fast) mode.
    *
    * @param src source value
    *
    * @since 0.7.0
    */
  final def transformFailFast(src: From): partial.Result[To] =
    transform(src, failFast = true)
}

object PartialTransformer {

  /** Construct ad-hoc instance of partial transformer from transforming function.
    *
    * @param f transforming function
    * @tparam A type of input value
    * @tparam B type of output value
    * @return [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  def apply[A, B](f: A => partial.Result[B]): PartialTransformer[A, B] =
    (src: A, _: Boolean) => {
      try {
        f(src)
      } catch {
        case why: Throwable => partial.Result.fromErrorThrowable(why)
      }
    }

  /** Provides [[io.scalaland.chimney.PartialTransformer]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.PartialTransformer]] type class definition
    *
    * @since 0.7.0
    * */
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
    *
    * @since 0.7.0
    * */
  def define[From, To]: PartialTransformerDefinition[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new PartialTransformerDefinition(Map.empty, Map.empty)

}
