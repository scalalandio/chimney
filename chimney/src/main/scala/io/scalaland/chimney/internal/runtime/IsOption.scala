package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.integrations.OptionalValue

import scala.annotation.{implicitNotFound, unused}

// $COVERAGE-OFF$evidence used only within macro-erased expressions

/** Allow us to provide `.matchingSome` extension method only for the types where macros would actually handle it.
  *
  * @since 1.0.0
  */
@implicitNotFound("Expected Option (type extending scala.Option) or OptionalValue, got ${O}")
sealed trait IsOption[O] {
  type SomeValue
  type Some
}
object IsOption extends IsOptionImplicits0 {
  @implicitNotFound("Expected Option (type extending scala.Option) or OptionalValue, got ${O}")
  type Of[O, SV, S] = IsOption[O] { type SomeValue = SV; type Some = S }

  protected object Impl extends IsOption[Nothing]
}

private[runtime] trait IsOptionImplicits0 extends IsOptionImplicits1 { this: IsOption.type =>

  // build-in Chimney support for Options is always provided
  implicit def optionIsOption[A]: IsOption.Of[Option[A], A, Some[A]] =
    Impl.asInstanceOf[IsOption.Of[Option[A], A, Some[A]]]

  // build-in Chimney support for Somes is always provided
  implicit def someIsOption[A]: IsOption.Of[Some[A], A, Some[A]] =
    Impl.asInstanceOf[IsOption.Of[Some[A], A, Some[A]]]
}
private[runtime] trait IsOptionImplicits1 { this: IsOption.type =>

  // OptionalValue is supported by design
  implicit def optionalValueIsOption[O, A](implicit @unused ev: OptionalValue[O, A]): IsOption.Of[O, A, Some[A]] =
    Impl.asInstanceOf[IsOption.Of[O, A, Some[A]]]
}
