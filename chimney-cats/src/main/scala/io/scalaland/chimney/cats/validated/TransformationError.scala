package io.scalaland.chimney.cats.validated

import cats.data.Chain

abstract class TransformationError(val message: String) {
  def errorPath: Chain[String]

  def addPrefix(prefix: String): TransformationError
}

object TransformationError {
  private case class Deep(override val message: String, path: NEC[String]) extends TransformationError(message) {
    lazy val errorPath: Chain[String] =
      path.toChain

    def addPrefix(prefix: String): TransformationError =
      Deep(message, path.prepend(prefix))
  }

  private case class Root(override val message: String) extends TransformationError(message) {
    val info: String =
      message

    val errorPath: Chain[String] =
      Chain.nil

    def addPrefix(prefix: String): TransformationError =
      Deep(message, NEC(prefix))
  }

  def apply(message: String): TransformationError =
    Root(message)
}
