package io.scalaland.chimney.utils

import scala.util.Try

object OptionUtils {

  implicit class StringOps(val s: String) extends AnyVal {
    def parseInt: Option[Int] = Try(s.toInt).toOption
    def parseDouble: Option[Double] = Try(s.toDouble).toOption
  }

}
