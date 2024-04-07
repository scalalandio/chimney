package io.scalaland.chimney.internal.runtime

sealed trait IsEither[E] {
  type Left
  type Right
}
object IsEither {

  type Of[E, L, R] = IsEither[E] { type Left = L; type Right = R }

  private object Impl extends IsEither[Nothing]

  implicit def eitherOsEither[L, R, E <: Left[L, R]]: IsEither.Of[E, L, R] = Impl.asInstanceOf[IsEither.Of[E, L, R]]
}
