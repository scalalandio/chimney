package io.scalaland.chimney.internal.runtime

// TODO @implicitNotFound
sealed trait IsEither[E] {
  type LeftValue
  type RightValue
  type Left
  type Right
}
object IsEither {
  type Of[E, LV, RV, L, R] = IsEither[E] { type LeftValue = LV; type RightValue = RV; type Left = L; type Right = R }

  private object Impl extends IsEither[Nothing]

  implicit def eitherOsEither[LV, RV, E <: Either[LV, RV]]: IsEither.Of[E, LV, RV, Left[LV, RV], Right[LV, RV]] =
    Impl.asInstanceOf[IsEither.Of[E, LV, RV, Left[LV, RV], Right[LV, RV]]]
}
