package io.scalaland.chimney

sealed trait ErrorPathNode {
  def show: String

  def separator: String
}

object ErrorPathNode {
  case class Accessor(name: String) extends ErrorPathNode {
    def show: String = name

    val separator: String = "."
  }

  case class Index(value: Int) extends ErrorPathNode {
    def show: String = s"[$value]"

    val separator: String = ""
  }
}
