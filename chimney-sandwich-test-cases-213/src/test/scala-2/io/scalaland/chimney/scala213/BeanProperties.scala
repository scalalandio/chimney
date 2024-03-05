package io.scalaland.chimney.scala213

import scala.beans.BeanProperty

object BeanProperties {
  final case class Foo private (
      @BeanProperty var a: Int,
      @BeanProperty var b: String,
      @BeanProperty var c: Double,
      @BeanProperty var d: Boolean
  ) { def this() = this(0, "", 0.0, false) }
  final case class Bar private (
      @BeanProperty var a: Int,
      @BeanProperty var b: String,
      @BeanProperty var c: Double
  ) { def this() = this(0, "", 0.0) }
  final case class Baz private (
      @BeanProperty var a: Int,
      @BeanProperty var b: String,
      @BeanProperty var c0: Double,
      @BeanProperty var d: Int
  ) { def this() = this(0, "", 0.0, 0) }
}
