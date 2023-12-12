package io.scalaland.chimney.cats

import _root_.cats.{Contravariant, Monad}
import _root_.cats.arrow.Arrow
import io.scalaland.chimney.Transformer

trait CatsTotalTransformerImplicits {

  // TODO: @since
  implicit final val arrowTransformer: Arrow[Transformer] = new Arrow[Transformer] {
    override def lift[A, B](f: A => B): Transformer[A, B] = f(_)

    override def first[A, B, C](fa: Transformer[A, B]): Transformer[(A, C), (B, C)] =
      pair => fa.transform(pair._1) -> pair._2

    override def compose[A, B, C](f: Transformer[B, C], g: Transformer[A, B]): Transformer[A, C] =
      a => f.transform(g.transform(a))
  }

  // TODO: @since
  implicit final def monadTransformer[Source]: Monad[Transformer[Source, *]] =
    new Monad[Transformer[Source, *]] {
      override def pure[A](x: A): Transformer[Source, A] = _ => x

      override def flatMap[A, B](fa: Transformer[Source, A])(f: A => Transformer[Source, B]): Transformer[Source, B] =
        src => f(fa.transform(src)).transform(src)

      override def tailRecM[A, B](a: A)(
          f: A => Transformer[Source, Either[A, B]]
      ): Transformer[Source, B] =
        src => {
          @scala.annotation.tailrec
          def loop(a1: A): B = f(a1).transform(src) match {
            case Left(a2) => loop(a2)
            case Right(b) => b
          }
          loop(a)
        }
    }

  // TODO: @since
  implicit final def contravariantTransformer[Target]: Contravariant[Transformer[*, Target]] =
    new Contravariant[Transformer[*, Target]] {
      def contramap[A, B](fa: Transformer[A, Target])(f: B => A): Transformer[B, Target] =
        b => fa.transform(f(b))
    }
}
