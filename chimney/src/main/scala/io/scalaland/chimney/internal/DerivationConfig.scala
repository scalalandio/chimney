package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait DerivationConfig {

  val c: blackbox.Context

  case class PastedTree(isFun: Boolean, tree: c.Tree)

  case class Config(disableDefaultValues: Boolean, fieldTrees: Map[String, PastedTree])
}
