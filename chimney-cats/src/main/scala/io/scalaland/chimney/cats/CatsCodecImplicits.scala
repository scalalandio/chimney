package io.scalaland.chimney.cats

import _root_.cats.arrow.Category
import cats.InvariantSemigroupal
import io.scalaland.chimney.Codec
import io.scalaland.chimney.partial

/** @since 1.3.0 */
trait CatsCodecImplicits {

  /** @since 1.3.0 */
  implicit final val catsCategoryForCodec: Category[Codec] =
    new Category[Codec] {
      override def id[A]: Codec[A, A] = Codec(identity[A], (a, _) => partial.Result.fromValue(a))

      override def compose[A, B, C](f: Codec[B, C], g: Codec[A, B]): Codec[A, C] = Codec(
        (domain: A) => f.encode.transform(g.encode.transform(domain)),
        (dto: C, failFast: Boolean) => f.decode.transform(dto, failFast).flatMap(g.decode.transform(_, failFast))
      )
    }

  /** @since 1.3.0 */
  implicit final def catsInvariantSemigroupalForCodec[Domain]: InvariantSemigroupal[Codec[Domain, *]] =
    new InvariantSemigroupal[Codec[Domain, *]] {
      override def imap[A, B](fa: Codec[Domain, A])(f: A => B)(g: B => A): Codec[Domain, B] = Codec(
        (domain: Domain) => f(fa.encode.transform(domain)),
        (b: B, failFast: Boolean) => fa.decode.transform(g(b), failFast)
      )

      override def product[A, B](fa: Codec[Domain, A], fb: Codec[Domain, B]): Codec[Domain, (A, B)] = Codec(
        (domain: Domain) => (fa.encode.transform(domain), fb.encode.transform(domain)),
        (ab: (A, B), failFast: Boolean) =>
          partial.Result
            .product(fa.decode.transform(ab._1, failFast), fb.decode.transform(ab._2, failFast), failFast)
            .map(_._1)
      )
    }
}
