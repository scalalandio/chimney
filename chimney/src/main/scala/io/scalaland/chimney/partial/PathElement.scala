package io.scalaland.chimney.partial

/** Data type for representing path element in a (possibly) nested object structure.
  *
  * @see [[https://chimney.readthedocs.io/supported-transformations/#total-transformers-vs-partialtransformers]]
  *
  * @since 0.7.0
  */
sealed trait PathElement {

  /** Obtain conventional string representation of path element\
    *
    * @since 0.7.0
    */
  def asString: String
}

/** Companion of [[io.scalaland.chimney.partial.PathElement]].
  *
  * @see [[https://chimney.readthedocs.io/supported-transformations/#total-transformers-vs-partialtransformers]]
  *
  * @since 0.7.0
  */
object PathElement {

  /** Object property accessor (e.g case class param name).
    *
    * @param name field name
    *
    * @since 0.7.0
    */
  final case class Accessor(name: String) extends PathElement {
    override def asString: String = name
  }

  /** Index in a collection.
    *
    * @param index index of an element
    *
    * @since 0.7.0
    */
  final case class Index(index: Int) extends PathElement {
    override def asString: String = s"($index)"
  }

  /** Value in the map.
    *
    * @param key key in the map
    *
    * @since 0.7.0
    */
  final case class MapValue(key: Any) extends PathElement {
    override def asString: String = s"($key)"
  }

  /** Key in the map.
    *
    * @param key key in the map
    *
    * @since 0.7.0
    */
  final case class MapKey(key: Any) extends PathElement {
    override def asString: String = s"keys($key)"
  }

  /** Specifies if path element in conventional string representation should be prepended with a dot.
    *
    * @param pathElement path element
    *
    * @since 0.7.0
    */
  final def shouldPrependWithDot(pathElement: PathElement): Boolean = pathElement match {
    case _: Accessor => true
    case _: Index    => false
    case _: MapValue => false
    case _: MapKey   => true
  }
}
