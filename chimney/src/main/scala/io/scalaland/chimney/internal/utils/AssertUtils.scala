package io.scalaland.chimney.internal.utils

import scala.reflect.macros.blackbox

trait AssertUtils extends CompanionUtils {

  val c: blackbox.Context

  def assertOrAbort(cond: Boolean, errMessage: => String): Unit = {
    if (!cond) {
      c.abort(c.enclosingPosition, errMessage)
    }
  }
}
