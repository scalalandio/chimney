package io.scalaland.chimney.internal.compiletime.fp

import scala.language.implicitConversions

private[compiletime] object Syntax {

  implicit def pureSyntax[A](a: A): Applicative.PureOps[A] = new Applicative.PureOps(a)

  implicit def functorSyntax[F[_], A](fa: F[A]): Functor.Ops[F, A] = new Functor.Ops(fa)

  implicit def applicativeSyntax[F[_], A](fa: F[A]): Applicative.Ops[F, A] = new Applicative.Ops(fa)

  implicit def traverseSyntax[F[_], A](fa: F[A]): Traverse.Ops[F, A] = new Traverse.Ops(fa)

  implicit def sequenceSyntax[F[_], G[_], A](fga: F[G[A]]): Traverse.SequenceOps[F, G, A] =
    new Traverse.SequenceOps(fga)
}
