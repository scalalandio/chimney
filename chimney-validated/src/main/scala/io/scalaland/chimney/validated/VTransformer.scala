package io.scalaland.chimney.validated

import cats.implicits._
import cats.data.{NonEmptyChain, ValidatedNec}
import io.scalaland.chimney.validated.internal.ChimneyVBlackboxMacros

import scala.language.experimental.macros

trait VTransformer[From, To] {
  def transform(src: From): ValidatedNec[VTransformer.Error, To]
}

object VTransformer {
  abstract class Error(val info: String) {
    def addPrefix(prefix: String): Error
  }

  object Error {
    private case class Deep(message: String, path: NonEmptyChain[String])
        extends Error(s"$message on ${path.toList.mkString(".")}") {
      def addPrefix(prefix: String): Error =
        Deep(message, path.prepend(prefix))
    }

    private case class Root(message: String) extends Error(message) {
      def addPrefix(prefix: String): Error =
        Deep(message, NonEmptyChain(prefix))
    }

    def apply(message: String): Error =
      Root(message)
  }

  def addPrefix[To](res: ValidatedNec[VTransformer.Error, To], prefix: String): ValidatedNec[VTransformer.Error, To] =
    res.leftMap(_.map(_.addPrefix(prefix)))

  def error[To](message: String): ValidatedNec[VTransformer.Error, To] =
    Error(message).invalidNec

  implicit def derive[From, To]: VTransformer[From, To] =
    macro ChimneyVBlackboxMacros.deriveVTransformerImpl[From, To]
}
