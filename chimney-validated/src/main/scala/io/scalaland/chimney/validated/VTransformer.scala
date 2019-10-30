package io.scalaland.chimney.validated

import cats.implicits._
import io.scalaland.chimney.validated.internal.ChimneyVBlackboxMacros
import io.scalaland.chimney.validated.aliases._

import scala.language.experimental.macros

trait VTransformer[From, To] {
  def transform(src: From): IV[To]

  def transformToV(src: From): V[To] =
    transform(src).leftMap(_.map(_.info))
}

object VTransformer {
  abstract class Error(val info: String) {
    def addPrefix(prefix: String): Error
  }

  object Error {
    private case class Deep(message: String, path: NEL[String])
        extends Error(s"$message on ${path.toList.mkString(".")}") {
      def addPrefix(prefix: String): Error =
        Deep(message, NEL(prefix, path.toList))
    }

    private case class Root(message: String) extends Error(message) {
      def addPrefix(prefix: String): Error =
        Deep(message, NEL.of(prefix))
    }

    def apply(message: String): Error =
      Root(message)
  }

  def addPrefix[A](iv: IV[A], prefix: String): IV[A] =
    iv.leftMap(_.map(_.addPrefix(prefix)))

  def error[A](message: String): IV[A] =
    Error(message).invalidNec

  implicit def derive[From, To]: VTransformer[From, To] =
    macro ChimneyVBlackboxMacros.deriveVTransformerImpl[From, To]
}
