package io.scalaland.chimney.partial

/** Data structure for representing path in a (possibly) nested object structure.
  *
  * @param elems list of path elements
  *
  * @since 0.7.0
  */
final case class Path(private val elems: List[PathElement]) extends AnyVal {

  /** Prepend of error path with a path element
    *
    * @param pathElement path element to be prepended
    * @return path with prepended path element
    *
    * @since 0.7.0
    */
  def prepend(pathElement: PathElement): Path = Path(pathElement :: elems)

  /** Returns conventional string based representation of a path
    *
    * @since 0.7.0
    */
  def asString: String =
    if (elems.isEmpty) ""
    else {
      val sb = new StringBuilder
      val it = elems.iterator
      while (it.hasNext) {
        val curr = it.next()
        if (sb.nonEmpty && PathElement.shouldPrependWithDot(curr)) {
          sb += '.'
        }
        sb ++= curr.asString
      }
      sb.result()
    }
}

object Path {

  /** Empty error path
    *
    * @since 0.7.0
    */
  final val Empty: Path = Path(Nil)
}
