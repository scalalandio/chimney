package io.scalaland.chimney

import scala.collection.compat._

/** Type class adding support or error path for lifted transformers.
  *
  * If you implement it, you will be able to get path of each error in transformation.
  *
  * @see [[TransformerFErrorPathSupport.TransformerFErrorPathEitherSupport]] for implementation for `Either[C[TransformationError], +*]`
  *
  * @tparam F wrapper type constructor
  */
trait TransformerFErrorPathSupport[F[+_]] {

  /** Prepend node of path to each error in wrapped value.
    *
    * @param fa wrapped value
    * @param node previous node of path
    * @tparam A type of value
    * @return wrapped value with added node in errors
    */
  def addPath[A](fa: F[A], node: ErrorPathNode): F[A]
}

object TransformerFErrorPathSupport {
  implicit def TransformerFErrorPathEitherSupport[M, C[X] <: IterableOnce[X]](
      implicit ef: Factory[TransformationError[M], C[TransformationError[M]]]
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
