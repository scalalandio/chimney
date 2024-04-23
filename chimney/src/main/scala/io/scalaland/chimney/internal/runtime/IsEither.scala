package io.scalaland.chimney.internal.runtime

import scala.annotation.implicitNotFound

// $COVERAGE-OFF$evidence used only within macro-erased expressions

/** Allow us to provide `.matchingLeft` and `.matchingRight` extension methods only for the types where macros would
  * actually handle it.
  *
  * @since 1.0.0
  */
@implicitNotFound("Expected Either (type extending scala.Either), got ${E}")
sealed trait IsEither[E] {
  type LeftValue
  type RightValue
  type Left
  type Right
}
object IsEither extends IsEitherImplicits0 {
  @implicitNotFound("Expected Either (type extending scala.Either), got ${E}")
  type Of[E, LV, RV, L, R] = IsEither[E] { type LeftValue = LV; type RightValue = RV; type Left = L; type Right = R }

  protected object Impl extends IsEither[Nothing]
}
private[runtime] trait IsEitherImplicits0 { this: IsEither.type =>

  // build-in Chimney support for Eithers is always provided
  implicit def eitherIsEither[LV, RV]: IsEither.Of[Either[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]] =
    Impl.asInstanceOf[IsEither.Of[Either[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]]]

  // build-in Chimney support for Lefts is always provided
  implicit def leftIsEither[LV, RV]: IsEither.Of[Left[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]] =
    Impl.asInstanceOf[IsEither.Of[Left[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]]]

  // build-in Chimney support for Rights is always provided
  implicit def rightIsEither[LV, RV]: IsEither.Of[Right[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]] =
    Impl.asInstanceOf[IsEither.Of[Right[LV, RV], LV, RV, Left[LV, RV], Right[LV, RV]]]
}
