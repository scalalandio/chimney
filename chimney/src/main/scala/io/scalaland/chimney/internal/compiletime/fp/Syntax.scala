package io.scalaland.chimney.internal.compiletime.fp

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

private[compiletime] object Syntax {

  implicit def pureSyntax[A](a: A): Applicative.PureOps[A] = new Applicative.PureOps(a)

  implicit def functorSyntax[F[_], A](fa: F[A]): Functor.Ops[F, A] = new Functor.Ops(fa)

  implicit def applicativeSyntax[F[_], A](fa: F[A]): Applicative.Ops[F, A] = new Applicative.Ops(fa)

  implicit def traverseSyntax[F[_], A](fa: F[A]): Traverse.Ops[F, A] = new Traverse.Ops(fa)

  implicit def sequenceSyntax[F[_], G[_], A](fga: F[G[A]]): Traverse.SequenceOps[F, G, A] =
    new Traverse.SequenceOps(fga)

  implicit val listApplicativeTraverse: ApplicativeTraverse[List] = new ApplicativeTraverse[List] {

    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.map2(f(a)) { (buffer: ListBuffer[B], b: B) => buffer.addOne(b) }
      }.map(_.toList)

    def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = fa.zip(fb).map(f.tupled)

    def pure[A](a: A): List[A] = List(a)
  }
}
