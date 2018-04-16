package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox
import scala.util.matching.Regex

trait Dsl2Macros {
  this: TransformerMacros with Dsl2Macros with MacroUtils with DerivationConfig =>

  val c: blackbox.Context

  import c.universe._

  def expandDisableDefaultValues: c.Tree = {

    c.prefix.tree.insertToBlock{
      q"val ${TermName(Prefixes.disableDefaults)} = true"
    }
  }

  def expandWithFieldConst(fieldName: c.Name, value: c.Tree): c.Tree = {
    val fieldNameStr = fieldName.decodedName.toString
    val memNameStr = TermName(s"${Prefixes.const}$fieldNameStr")

    c.prefix.tree.insertToBlock {
      q"val $memNameStr = $value"
    }
  }

  def expandWithFieldComputed(fieldName: c.Name, map: c.Tree): c.Tree = {
    val fieldNameStr = fieldName.decodedName.toString
    val memNameStr = TermName(s"${Prefixes.computed}$fieldNameStr")

    c.prefix.tree.insertToBlock {
      q"val $memNameStr = $map"
    }
  }

  def expandTansform[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    println("expandTansform: " + c.prefix.tree)

    val Typed(Block(stats, expr), _) = c.prefix.tree

    stats.foreach(println)
    println(expr)

    val q"io.scalaland.chimney.dsl2.TransformerOps[$pFrom]($source).into[$pTo]" = expr

    println(s"pFrom: $pFrom")
    println(s"pTo: $pTo")
    println(s"source: $source")

    val config = captureConfiguration(stats)

    val derivedTransformerTree = genTransformer[From, To](config).tree

    q"$derivedTransformerTree.transform($source)"
  }

  def captureConfiguration(stats: List[c.Tree]): Config = {

    val config = Config(disableDefaultValues = false, consts = Map.empty, funs = Map.empty)

    stats.foldLeft(config) { case (cfg, stat) =>
      stat match {
        case ValDef(_, TermName(memName), _, tree) =>
          memName match {
            case Prefixes.disableDefaults =>
              cfg.copy(disableDefaultValues = true)
            case Prefixes.ConstPat(fieldName) =>
              cfg.copy(consts = cfg.consts + (fieldName -> tree))
            case Prefixes.ComputedPat(fieldName) =>
              cfg.copy(funs = cfg.funs + (fieldName -> tree))
          }
        case _ =>
          cfg
      }
    }
  }

  private object Prefixes {
    val disableDefaults: String = "__chimney$disableDefaultValues"
    val const: String = "__chimney$const_"
    val computed: String = "__chimney$computed_"

    val ConstPat: Regex = """^\_\_chimney\$const\_(.*)$""".r
    val ComputedPat: Regex = """^\_\_chimney\$computed\_(.*)$""".r
  }
}
