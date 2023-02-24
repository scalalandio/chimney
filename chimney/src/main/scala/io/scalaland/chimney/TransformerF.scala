package io.scalaland.chimney

import io.scalaland.chimney.dsl.{TransformerDefinitionCommons, TransformerFDefinition}
import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros

import scala.language.experimental.macros

/** Type class expressing partial transformation between
  * source type `From` and target type `To`, wrapping
  * transformation result in type constructor `F`.
  *
  * Useful for validated transformations, where result
  * type is wrapped in Option, Either, Validated, etc...
  *
  * @deprecated migration described at [[https://scalalandio.github.io/chimney/partial-transformers/migrating-from-lifted.html]]
  *
  * @see [[io.scalaland.chimney.TransformerFSupport]]
  *
  * @tparam F    wrapper type constructor
  * @tparam From type of input value
  * @tparam To   type of output value
  *
  * @since 0.5.0
  */
@deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
trait TransformerF[F[+_], From, To] {

  /** Transforms value with some effect F.
    *
    * Should be a referentially transparent function.
    *
    * @param src source value
    * @return transformed value wrapped in F
    *
    * @since 0.5.0
    */
  def transform(src: From): F[To]
}

@deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
object TransformerF {

  /** Provides [[io.scalaland.chimney.TransformerF]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam F    wrapper type constructor
    * @tparam From type of input value
    * @tparam To   type of output value
    * @return [[io.scalaland.chimney.TransformerF]] type class definition
    *
    * @since 0.5.0
    */
  implicit def derive[F[+_], From, To](implicit tfs: TransformerFSupport[F]): TransformerF[F, From, To] =
    macro TransformerBlackboxMacros.deriveTransformerFImpl[F, From, To]

  /** Creates an empty [[io.scalaland.chimney.dsl.TransformerFDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.TransformerF]].
    *
    * @see [[io.scalaland.chimney.dsl.TransformerFDefinition]] for available settings
    *
    * @tparam F    wrapper type constructor
    * @tparam From type of input value
    * @tparam To   type of output value
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]] with defaults
    *
    * @since 0.5.0
    */
  def define[F[+_], From, To]
      : TransformerFDefinition[F, From, To, TransformerCfg.WrapperType[F, TransformerCfg.Empty], TransformerFlags.Default] =
    new TransformerFDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore)

}
