package io.scalaland.chimney

import scala.collection.compat.*

/** Type class adding support or error path for lifted transformers.
  *
  * If you implement it, you will be able to get path of each error in transformation.
  *
  * @deprecated migration described at [[https://scalalandio.github.io/chimney/partial-transformers/migrating-from-lifted.html]]
  *
  * @see [[TransformerFErrorPathSupport.TransformerFErrorPathEitherSupport]] for implementation for `Either[C[TransformationError], +*]`
  *
  * @tparam F wrapper type constructor
  *
  * @since 0.6.1
  */
@deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
trait TransformerFErrorPathSupport[F[+_]] {

  /** Prepend node of path to each error in wrapped value.
    *
    * @param fa wrapped value
    * @param node previous node of path
    * @tparam A type of value
    * @return wrapped value with added node in errors
    *
    * @since 0.6.1
    */
  def addPath[A](fa: F[A], node: ErrorPathNode): F[A]
}

/** @since 0.6.1 */
@deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
object TransformerFErrorPathSupport {

  /** @since 0.6.1 */
  implicit def TransformerFErrorPathEitherSupport[M, C[X] <: IterableOnce[X]](implicit
      ef: Factory[TransformationError[M], C[TransformationError[M]]]
  ): TransformerFErrorPathSupport[Either[C[TransformationError[M]], +*]] =
    new TransformerFErrorPathSupport[Either[C[TransformationError[M]], +*]] {
      def addPath[A](
          fa: Either[C[TransformationError[M]], A],
          node: ErrorPathNode
      ): Either[C[TransformationError[M]], A] =
        fa match {
          case Left(errors) => Left(ef.fromSpecific(errors.iterator.map(_.prepend(node))))
          case right        => right
        }
    }
}
