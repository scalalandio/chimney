package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.partial.Result.Errors

object ResultUtils {

  @scala.annotation.nowarn("msg=deprecated") // this method is rewritten in 2.0.0
  def mergeNullable[A](errorsNullable: Errors, result: Result[A]): Errors =
    result match {
      case _: Result.Value[?]    => errorsNullable
      case errors: Result.Errors => if (errorsNullable == null) errors else Result.Errors.merge(errorsNullable, errors)
    }
}
