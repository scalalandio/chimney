package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait Dsl2Macros {
  this: TransformerMacros with Dsl2Macros with MacroUtils with DerivationConfig =>

  val c: blackbox.Context

  import c.universe._

  def expandDisableDefaultValues: c.Tree = {

    c.prefix.tree.insertToBlock{
      q"val __chimney__disableDefaultValues = true"
    }
  }

  def expandWithFieldConst(fieldName: c.Name, value: c.Tree): c.Tree = {
    val fieldNameStr = fieldName.decodedName.toString
    val memNameStr = TermName(s"__chimney__const_$fieldNameStr")

    c.prefix.tree.insertToBlock {
      q"val $memNameStr = $value"
    }
  }

  def expandWithFieldComputed(fieldName: c.Name, map: c.Tree): c.Tree = {
    val fieldNameStr = fieldName.decodedName.toString
    val memNameStr = TermName(s"__chimney__computed_$fieldNameStr")

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

    val config = Config(disableDefaultValues = false, consts = Map.empty, funs = Map.empty)

    val derivedTransformerTree = genTransformer[From, To](config).tree

    q"$derivedTransformerTree.transform($source)"
  }
}
