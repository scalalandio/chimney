package io.scalaland.chimney

import io.scalaland.chimney.internal.ChimneyBlackboxMacros
import scala.language.experimental.macros

trait Patcher[T, Patch] {
  def patch(obj: T, patch: Patch): T
}

object Patcher {

  implicit def derive[T, Patch]: Patcher[T, Patch] =
    macro ChimneyBlackboxMacros.derivePatcherImpl[T, Patch]
}
