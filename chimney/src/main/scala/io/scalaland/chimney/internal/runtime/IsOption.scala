package io.scalaland.chimney.internal.runtime

// TODO: @implicitNotFound
sealed trait IsOption[O] {
  type SomeValue
  type Some
}
object IsOption {

  type Of[O, SV, S] = IsOption[O] { type SomeValue = SV; type Some = S }

  private object Impl extends IsOption[Nothing]

  implicit def optionIsOption[A, O <: Option[A]]: IsOption.Of[O, A, Some[A]] =
    Impl.asInstanceOf[IsOption.Of[O, A, Some[A]]]
}
