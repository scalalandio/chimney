package io.scalaland.chimney.cats

import _root_.cats.arrow.Category
import cats.InvariantSemigroupal
import io.scalaland.chimney.Iso

/** @since 1.3.0 */
trait CatsIsoImplicits {

  /** @since 1.3.0 */
  implicit final val catsCategoryForIso: Category[Iso] =
    new Category[Iso] {
      override def id[A]: Iso[A, A] = Iso(identity[A], identity[A])

      override def compose[A, B, C](f: Iso[B, C], g: Iso[A, B]): Iso[A, C] = Iso(
        (first: A) => f.first.transform(g.first.transform(first)),
        (second: C) => g.second.transform(f.second.transform(second))
      )
    }

  /** @since 1.3.0 */
  implicit final def catsInvariantSemigroupalForIso[First]: InvariantSemigroupal[Iso[First, *]] =
    new InvariantSemigroupal[Iso[First, *]] {
      override def imap[A, B](fa: Iso[First, A])(f: A => B)(g: B => A): Iso[First, B] = Iso(
        (first: First) => f(fa.first.transform(first)),
        (b: B) => fa.second.transform(g(b))
      )

      override def product[A, B](fa: Iso[First, A], fb: Iso[First, B]): Iso[First, (A, B)] = Iso(
        (first: First) => fa.first.transform(first) -> fb.first.transform(first),
        (ab: (A, B)) => fa.second.transform(ab._1)
      )
    }
}
