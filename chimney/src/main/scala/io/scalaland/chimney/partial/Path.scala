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
    // Const shouldn't be modified (or we can overwrite several nested Consts with outer one, which is useless)
    case (_: PathElement.Const) :: _    => this
    case (h: PathElement.Computed) :: t =>
      // sealed Computed should not be modified
      if (h.sealPath || pathElement.isInstanceOf[PathElement.Computed]) this
      // Computed inside Const should stay Computed - but it should be sealed again
      else if (pathElement.isInstanceOf[PathElement.Const]) {
        h.sealPath = true
        this
      }
      // Unsealed Computed can be prepended
      else Path(h :: pathElement :: t)
    case _ =>
      // If something prepends Const it should replace the content
      if (pathElement.isInstanceOf[PathElement.Const]) Path(pathElement :: Nil)
      else Path(pathElement :: elements)
  }

  /** Unseals the [[io.scalaland.chimney.partial.Path]] of current [[io.scalaland.chimney.partial.Error]].
    *
    * When derivation is building up the result it automatically appends fields/indices/map keys - however values
    * obtained with withFieldComputed(Partial)(From) contains the whole Path already, so [[prepend]] should be a noop
    * for them.
    *
    * However, this path can only be precomputed only up to the boundaries of a
    * [[io.scalaland.chimney.PartialTransformer]], and when one transformer calls another, path should be appended
    * again. This method allows this.
    *
    * @since 1.6.0
    */
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
        if (computedSuffix == null && (curr.isInstanceOf[PathElement.Computed])) {
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
