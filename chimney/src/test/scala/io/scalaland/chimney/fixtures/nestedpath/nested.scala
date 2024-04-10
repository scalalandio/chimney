package io.scalaland.chimney.fixtures.nestedpath

import scala.collection.immutable.ListMap

case class NestedProduct[A](value: A)

class NestedValueClass[A](val value: A) extends AnyVal
object NestedValueClass {
  def apply[A](value: A): NestedValueClass[A] = new NestedValueClass[A](value)
}

class NestedJavaBean[A] {
  private var value: A = null.asInstanceOf[A]
  def getValue: A = value
  def setValue(value: A): Unit = this.value = value

  override def equals(obj: Any): Boolean = obj match {
    case njb: NestedJavaBean[?] => value == njb.value
    case _                      => false
  }
}
object NestedJavaBean {
  def apply[A](value: A): NestedJavaBean[A] = {
    val jb = new NestedJavaBean[A]
    jb.setValue(value)
    jb
  }
}

case class NestedComplex[A](
    id: A,
    option: Option[A],
    either: Either[A, A],
    collection: List[A],
    map: ListMap[A, A]
)

sealed trait NestedADT[A]
object NestedADT {
  case class Foo[A](foo: A) extends NestedADT[A]
  case class Bar[A](bar: A) extends NestedADT[A]
}
