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

  case class MapKey[K](key: K) extends ErrorPathNode {
    def show: String = s"[$key]"

    val separator: String = ""
  }

  case object MapKeys extends ErrorPathNode {
    val show: String = "keys"

    val separator: String = "."
  }
}
