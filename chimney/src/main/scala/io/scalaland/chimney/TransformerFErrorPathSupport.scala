package io.scalaland.chimney

import scala.collection.compat.{Factory, IterableOnce}

trait TransformerFErrorPathSupport[F[+_]] {
  def addPath[A](fa: F[A], node: ErrorPathNode): F[A]
}

object TransformerFErrorPathSupport {
  implicit def transformerFErrorPathEitherSupport[M, C[X] <: IterableOnce[X]](
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
