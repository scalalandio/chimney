package io.scalaland.chimney

import scala.collection.compat._

trait TransformerFFromOptionSupport[F[+_]] {

  /** Create F[B] from Option[A]
    *
    * @tparam A type of wrapped value
    * @param opt wrapped in option value
    * @param trans wrapped transformation function from A to B in F
    * @return wrapped in F result of transformation option unwrapping
    */
  def fromOption[A, B](opt: Option[A], trans: A => F[B]): F[B]
}

object TransformerFFromOptionSupport {
  implicit val OptionInstance: TransformerFFromOptionSupport[Option] = new TransformerFFromOptionSupport[Option] {
    override def fromOption[A, B](opt: Option[A], trans: A => Option[B]): Option[B] =
      opt.flatMap(trans)
  }

  implicit def EitherStringInstance[C[X] <: IterableOnce[X], M](
      implicit ef: Factory[TransformationError[M], C[TransformationError[M]]]
  ): TransformerFFromOptionSupport[Either[C[TransformationError[M]], +*]] =
    new TransformerFFromOptionSupport[Either[C[TransformationError[M]], +*]] {
      override def fromOption[A, B](
          opt: Option[A],
          trans: A => Either[C[TransformationError[M]], B]
      ): Either[C[TransformationError[M]], B] =
        opt.fold[Either[C[TransformationError[M]], B]](
          Left(ef.fromSpecific(List(TransformationError.OptionUnwrappingError)))
        )(trans)
    }
}
