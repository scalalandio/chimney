package io.scalaland.chimney.internal.runtime

import scala.annotation.implicitNotFound

@implicitNotFound("Expected Option (type extending scala.Option), got ${O}")
sealed trait IsOption[O] {
  type SomeValue
  type Some
}
object IsOption {
  @implicitNotFound("Expected Option (type extending scala.Option), got ${O}")
  type Of[O, SV, S] = IsOption[O] { type SomeValue = SV; type Some = S }

  private object Impl extends IsOption[Nothing]

  implicit def optionIsOption[A]: IsOption.Of[Option[A], A, Some[A]] =
    Impl.asInstanceOf[IsOption.Of[Option[A], A, Some[A]]]

  implicit def someIsOption[A]: IsOption.Of[Some[A], A, Some[A]] =
    Impl.asInstanceOf[IsOption.Of[Some[A], A, Some[A]]]
}
