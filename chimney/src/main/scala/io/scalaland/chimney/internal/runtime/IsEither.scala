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

  implicit def eitherIsEither[LV, RV]: IsEither.Of[Either[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]] =
    Impl.asInstanceOf[IsEither.Of[Either[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]]]

  implicit def leftIsEither[LV, RV]: IsEither.Of[Left[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]] =
    Impl.asInstanceOf[IsEither.Of[Left[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]]]

  implicit def rightIsEither[LV, RV]: IsEither.Of[Right[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]] =
    Impl.asInstanceOf[IsEither.Of[Right[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]]]
}
