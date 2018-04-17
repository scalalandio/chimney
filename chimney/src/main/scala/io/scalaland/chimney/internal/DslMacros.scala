package io.scalaland.chimney.internal

import scala.reflect.macros.whitebox

trait DslMacros {
  this: TransformerMacros with DslMacros with MacroUtils with DerivationConfig with Prefixes =>

  val c: whitebox.Context

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

  def expandTansform[From: c.WeakTypeTag, To: c.WeakTypeTag, Overrides: c.WeakTypeTag]: c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val Overrides = weakTypeOf[Overrides]

    val srcName = TermName(c.freshName("src"))

    val config = captureConfiguration(Overrides)
    println(s"RUNNIN WITH CONFIG: $config")
    val derivedTransformerTree = genTransformer[From, To](config).tree

    q"""
       val $srcName = ${c.prefix.tree}.source
       $derivedTransformerTree.transform($srcName)
     """
  }

  def captureConfiguration(overridesTpe: Type, config: Config = Config(disableDefaultValues = false, overridenFields = Set.empty)): Config = {

    config

//    overridesTpe match {
//      case tq"Nl" => config
//      case tq"Cns[FieldConst[$fieldName], $rest]" =>
//        captureConfiguration(rest.tpe.asInstanceOf[Type], config.copy(overridenFields = config.overridenFields + fieldName.toString()))
//      case tq"Cns[FieldComputed[$fieldName], $rest]" =>
//        captureConfiguration(rest.tpe.asInstanceOf[Type], config.copy(overridenFields = config.overridenFields + fieldName.toString()))
//      case _ =>
//        c.abort(c.enclosingPosition, "Bad overriden type shape!")
//    }
  }
}
