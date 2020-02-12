package io.scalaland.chimney.cats.validated

abstract class TransformationError(val info: String) {
  def addPrefix(prefix: String): TransformationError
}

object TransformationError {
  private case class Deep(message: String, path: NEC[String])
      extends TransformationError(s"$message on ${path.toChain.iterator.mkString(".")}") {
    def addPrefix(prefix: String): TransformationError =
      Deep(message, path.prepend(prefix))
  }

  private case class Root(message: String) extends TransformationError(message) {
    def addPrefix(prefix: String): TransformationError =
      Deep(message, NEC(prefix))
  }

  def apply(message: String): TransformationError =
    Root(message)
}
