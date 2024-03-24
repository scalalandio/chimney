package io.scalaland.chimney.integrations

trait OptionalOf[F[_]] {
  def someOf[A](a: A): F[A]
  
  def noneOf[A]: F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def getOrElse[A](fa: F[A])(orElse: => A): A
}

// TODO: 
// in dsl/auto - Transformer.AutoDerived/PartialTransformer.AutoDerived
// in companion object? - Transformer.OrUpcast/PartialTransformer.OrUpcast
// operations:
// - Total Optional -> Optional
// - Total Optional -> Option
// - Total Option -> Optional
// - Total pure -> Optional
// - Partial Optional -> pure
// - low priority
// - Partial Optional -> Optional
// - Partial Optional -> Option
// - Partial Option -> Optional
// - Partial pure -> Optional

