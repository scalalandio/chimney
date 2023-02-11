package io.scalaland.chimney.partial

sealed trait PathElement {
  def asString: String
}

object PathElement {
  final case class Accessor(name: String) extends PathElement {
    override def asString: String = name
  }

  final case class Index(index: Int) extends PathElement {
    override def asString: String = s"($index)"
  }

  final case class MapValue(key: Any) extends PathElement {
    override def asString: String = s"($key)"
  }

  final case class MapKey(key: Any) extends PathElement {
    override def asString: String = s"keys($key)"
  }

  final def shouldPrependWithDot(pe: PathElement): Boolean = pe match {
    case _: Accessor => true
    case _: Index => false
    case _: MapValue => false
    case _: MapKey => true
  }
}
