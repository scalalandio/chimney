package io.scalaland.chimney.internal

import io.scalaland.chimney.dsl2._

private[chimney] class Dsl2Macros(val c: scala.reflect.macros.blackbox.Context)
  extends MacroUtils {

  import c.universe._

  def disableDefaultValuesImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[TransformerInto[From, To]] = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    val ttt = c.prefix.tree.insertToBlock {
      q"val __chimney__disableDefaultValues = true"
    }.debug

    println("disableDefaultValuesImpl: " + ttt)

    c.Expr[TransformerInto[From, To]](ttt)
  }

  def withFieldConstImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, T: c.WeakTypeTag](selector: c.Expr[To => T], value: c.Expr[T]): c.Expr[TransformerInto[From, To]] = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val T = weakTypeOf[T]

    println(selector)

    selector.tree match {
      case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: Name}" if vd.name == idt.name =>

        val fieldNameStr = fieldName.decodedName.toString
        val memNameStr = TermName(s"__chimney__const_$fieldNameStr")

        val ttt = c.prefix.tree.insertToBlock {
          q"val $memNameStr = $value"
        }.debug

        c.Expr[TransformerInto[From, To]](ttt)

      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def withFieldComputedImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, T: c.WeakTypeTag](selector: c.Expr[To => T], map: c.Expr[From => T]): c.Expr[TransformerInto[From, To]] = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val T = weakTypeOf[T]

    println(selector)

    selector.tree match {
      case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: Name}" if vd.name == idt.name =>

        val fieldNameStr = fieldName.decodedName.toString
        val memNameStr = TermName(s"__chimney__computed_$fieldNameStr")

        val ttt = c.prefix.tree.insertToBlock {
          q"val $memNameStr = $map"
        }.debug

        c.Expr[TransformerInto[From, To]](ttt)

      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def transformImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[To] = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]


    println("transformImpl: " + c.prefix.tree)


    val Typed(Block(stats, expr), _) = c.prefix.tree

    stats.foreach(println)
    println(expr)

    val q"io.scalaland.chimney.dsl2.TransformerOps[$pFrom]($source).into[$pTo]" = expr

    println(s"pFrom: $pFrom")
    println(s"pTo: $pTo")
    println(s"source: $source")

    val ttt =
      q"""
         $source.transformInto[$pTo]
       """
    c.Expr[To](ttt)
//    val ttt = c.prefix.tree.insertToBlock {
//      q"val __chimney__disableDefaultValues = true"
//    }.debug
//
//    println("disableDefaultValuesImpl: " + ttt)
//
//    c.Expr[TransformerInto[From, To]](ttt)
  }

}
