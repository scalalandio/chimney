package io.scalaland.chimney.partial

/** Data structure for representing path in a (possibly) nested object structure.
  *
  * @param elements
  *   list of path elements
  *
  * @since 0.7.0
  */
final case class Path(private val elements: List[PathElement]) extends AnyVal {

  /** Prepend of error path with a path element.
    *
    * @param pathElement
    *   path element to be prepended
    * @return
    *   path with prepended path element
    *
    * @since 0.7.0
    */
  def prepend(pathElement: PathElement): Path = elements match {
    case (_: PathElement.Const) :: _    => this
    case (h: PathElement.Computed) :: t => if (h.sealPath) this else Path(h :: pathElement :: t)
    case _                              => Path(pathElement :: elements)
  }

  def unsealPath(): Unit = elements match {
    case (h: PathElement.Computed) :: _ => h.sealPath = false
    case _                              =>
  }

  /** Returns conventional String-based representation of a path.
    *
    * @since 0.7.0
    */
  def asString: String =
    if (elements.isEmpty) ""
    else {
      val sb = new StringBuilder
      val it = elements.iterator
      // PathElement.Computed should always be the first element if present
      var computedSuffix: String = null
      while (it.hasNext) {
        val curr = it.next()
        if (computedSuffix == null && curr.isInstanceOf[PathElement.Computed]) {
          computedSuffix = curr.asString
        } else {
          if (sb.nonEmpty && PathElement.shouldPrependWithDot(curr)) {
            sb += '.'
          }
          sb ++= curr.asString
        }
      }
      if (computedSuffix != null) {
        if (sb.nonEmpty) {
          sb ++= " => "
        }
        sb ++= computedSuffix
      }
      sb.result()
    }
}

/** Companion of [[io.scalaland.chimney.partial.Path]].
  *
  * @since 0.7.0
  */
object Path {

  /** Empty error path
    *
    * @since 0.7.0
    */
  final val Empty: Path = Path(Nil)
}
