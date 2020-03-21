package io.scalaland.chimney

/** Default implementation of error with path info
  * @tparam M         type of error message
  * @param  message   error message value
  * @param  errorPath error location
  */
case class TransformationError[M](message: M, errorPath: List[ErrorPathNode] = Nil) {
  def prepend(node: ErrorPathNode): TransformationError[M] =
    TransformationError[M](message, node :: errorPath)

  def showErrorPath: String =
    errorPath match {
      case head :: tail =>
        tail.foldLeft(head.show)((acc, next) => acc + next.separator + next.show)
      case Nil => ""
    }
}
