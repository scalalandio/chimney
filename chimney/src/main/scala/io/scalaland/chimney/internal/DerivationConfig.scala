package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait DerivationConfig {

  val c: blackbox.Context

  case class Config(disableDefaultValues: Boolean,
                    consts: Map[String, c.Tree],
                    funs: Map[String, c.Tree])
}
