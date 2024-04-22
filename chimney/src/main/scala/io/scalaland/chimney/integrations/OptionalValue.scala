package io.scalaland.chimney.integrations

trait OptionalValue[Optional, Value] {

  def empty: Optional

  def of(value: Value): Optional

  def fold[A](oa: Optional, onNone: => A, onSome: Value => A): A

  def getOrElse(oa: Optional, onNone: => Value): Value = fold(oa, onNone, identity)

  def orElse(oa: Optional, onNone: => Optional): Optional = fold(oa, onNone, _ => oa)
}
