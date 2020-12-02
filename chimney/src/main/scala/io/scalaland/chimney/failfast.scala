package io.scalaland.chimney

import scala.collection.compat.Factory

object failfast {

  /** `TransformerFSupport` instance for `Either[E, +*]` that fails with first encountered error instead of accumulating like
    * [[TransformerFSupport.TransformerFEitherErrorAccumulatingSupport]] does.
    *
    * @tparam E error type
    */
  implicit def TransformerFEitherShortCircuitingSupport[E]: TransformerFSupport[Either[E, +*]] =
    new TransformerFSupport[Either[E, +*]] {

      override def pure[A](value: A): Either[E, A] = Right(value)

      override def product[A, B](fa: Either[E, A], fb: => Either[E, B]): Either[E, (A, B)] = {
        fa.flatMap(a => fb.map((a, _)))
      }

      override def map[A, B](fa: Either[E, A], f: A => B): Either[E, B] = {
        fa.map(f)
      }

      override def traverse[M, A, B](it: Iterator[A], f: A => Either[E, B])(
          implicit fac: Factory[B, M]
      ): Either[E, M] = {
        val bs = fac.newBuilder
        while (it.hasNext) {
          f(it.next()) match {
            case Left(err) => return Left(err)
            case Right(b) =>
              bs += b
          }
        }
        Right(bs.result())
      }
    }

}
