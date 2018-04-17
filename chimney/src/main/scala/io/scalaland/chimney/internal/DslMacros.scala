package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox
import scala.util.matching.Regex

trait DslMacros {
  this: TransformerMacros with DslMacros with MacroUtils with DerivationConfig with Prefixes =>

  val c: blackbox.Context

  import c.universe._

  def expandDisableDefaultValues: c.Tree = {

    c.prefix.tree.insertToBlock {
      q"val ${TermName(Prefixes.disableDefaults)} = true"
    }
  }

  def expandWithFieldConst(fieldName: c.Name, value: c.Tree): c.Tree = {
    val fieldNameStr = fieldName.decodedName.toString

    c.prefix.tree.insertToBlock {
      q"val ${constRefName(fieldNameStr)} = $value"
    }
  }

  def expandWithFieldComputed(fieldName: c.Name, map: c.Tree): c.Tree = {
    val fieldNameStr = fieldName.decodedName.toString

    c.prefix.tree.insertToBlock {
      q"val ${computedRefName(fieldNameStr)} = $map"
    }.debug
  }

  def expandTansform[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    println("expandTansform: " + c.prefix.tree)

    val (stats, expr) = c.prefix.tree.extractBlock

    stats.foreach(println)
    println(expr)

    val q"$transformerOpsTpe[$pFrom]($source).into[$pTo]" = expr

    println(s"TPE: $transformerOpsTpe")
    println(s"pFrom: $pFrom")
    println(s"pTo: $pTo")
    println(s"source: $source")

    val config = captureConfiguration(stats)
    val derivedTransformerTree = genTransformer[From, To](config).tree

    q"$derivedTransformerTree.transform($source)"
  }

  def captureConfiguration(stats: List[c.Tree]): Config = {

    val config = Config(disableDefaultValues = false, fieldTrees = Map.empty)

    stats.foldLeft(config) {
      case (cfg, stat) =>
        stat match {
          case ValDef(_, TermName(memName), _, tree) =>
            memName match {
              case Prefixes.disableDefaults =>
                cfg.copy(disableDefaultValues = true)
              case Prefixes.ConstPat(fieldName) =>
                val pastedTree = PastedTree(isFun = false, tree)
                cfg.copy(fieldTrees = cfg.fieldTrees + (fieldName -> pastedTree))
              case Prefixes.ComputedPat(fieldName) =>
                val pastedTree = PastedTree(isFun = true, tree)
                cfg.copy(fieldTrees = cfg.fieldTrees + (fieldName -> pastedTree))
            }
          case _ =>
            cfg
        }
    }
  }
}
