package io.scalaland.chimney.partial

final case class ErrorPath(private val elems: List[PathElement]) extends AnyVal {
  def prepend(pathElement: PathElement): ErrorPath = ErrorPath(pathElement :: elems)

  def asString: String = {
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
}

object ErrorPath {
  final val Empty: ErrorPath = ErrorPath(Nil)
}
