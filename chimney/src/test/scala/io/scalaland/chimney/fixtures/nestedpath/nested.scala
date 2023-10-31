package io.scalaland.chimney.fixtures.nestedpath

case class NestedProduct[A](value: A)

class NestedValueClass[A](val value: A) extends AnyVal

class NestedJavaBean[A] {
  private var value: A = null.asInstanceOf[A]
  def getValue: A = value
  def setValue(value: A): Unit = this.value = value
}
