package io.scalaland.chimney.internal.compiletime.derivation.transformer.datatypes

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait OptionalValues { this: Derivation =>

  abstract protected class OptionalValue[Optional, Value] {

    def empty: Optional

    def of(value: Value): Optional

    def fold[A](oa: Optional, onNone: => A, onSome: Value => A): A

    def getOrElse(oa: Optional, onNone: => Value): Value

    def orElse(oa: Optional, onNone: => Optional): Optional
  }
  object OptionalValue {

    def unapply[Optional](implicit Optional: Type[Optional]): Option[Existential[OptionalValue[Optional, *]]] = ???
  }
}
