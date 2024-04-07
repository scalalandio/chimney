package io.scalaland.chimney.internal.runtime

sealed trait IsOption[O] {
  type Value
}
object IsOption {

  type Of[O, A] = IsOption[O] { type Value = A }

  private object Impl extends IsOption[Nothing]

  implicit def optionIsOption[A, O <: Option[A]]: IsOption.Of[O, A] = Impl.asInstanceOf[IsOption.Of[O, A]]
}
