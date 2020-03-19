package io.scalaland.chimney

case class TransformationError[M](message: M, errorPath: List[ErrorPathNode] = Nil) {
  def prepend(node: ErrorPathNode): TransformationError[M] =
    TransformationError[M](message, errorPath.prepended(node))

  def showErrorPath: String =
    errorPath match {
      case head :: tail =>
        tail.foldLeft(head.show)((acc, next) => acc + next.separator + next.show)
      case Nil => ""
    }
}
