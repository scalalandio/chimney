package io.scalaland.chimney.internal.utils

import scala.reflect.macros.blackbox

trait AssertUtils extends CompanionUtils {

  val c: blackbox.Context

  def assertOrAbort(cond: Boolean, errMessage: => String): Unit = {
    // $COVERAGE-OFF$
    if (!cond) {
      c.abort(c.enclosingPosition, errMessage)
    }
    // $COVERAGE-OFF$
  }
}
