package io.scalaland.chimney.internal

import scala.reflect.macros.whitebox
import scala.util.matching.Regex

trait Prefixes {

  val c: whitebox.Context

  import c.universe._

  object Prefixes {
    val disableDefaults: String = "__chimney$disableDefaultValues"
    val const: String = "__chimney$const_"
    val computed: String = "__chimney$computed_"

    val ConstPat: Regex = """^\_\_chimney\$const\_(.*)$""".r
    val ComputedPat: Regex = """^\_\_chimney\$computed\_(.*)$""".r
  }

  def constRefName(name: String): TermName =
    TermName(s"${Prefixes.const}$name")

  def computedRefName(name: String): TermName =
    TermName(s"${Prefixes.computed}$name")

}
