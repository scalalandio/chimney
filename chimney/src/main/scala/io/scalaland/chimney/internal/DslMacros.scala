package io.scalaland.chimney.internal

import scala.reflect.macros.whitebox

trait DslMacros {
  this: TransformerMacros with DslMacros with MacroUtils with DerivationConfig with Prefixes =>

  val c: whitebox.Context

  import c.universe._

  def expandTansform[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]

    val srcName = c.freshName("src")

    val config = captureConfiguration(C).copy(prefixValName = srcName)

    println(s"RUNNIN WITH CONFIG: $config")

    val derivedTransformerTree = genTransformer[From, To](config).tree

    q"""
       val ${TermName(srcName)} = ${c.prefix.tree}
       $derivedTransformerTree.transform(${TermName(srcName)}.source)
    """.debug
  }

  def captureConfiguration(
    overridesTpe: Type,
    config: Config = Config(disableDefaultValues = false, overridenFields = Set.empty)
  ): Config = {

    val emptyT = typeOf[Empty]
    val disableDefaultsT = typeOf[DisableDefaults[_]].typeConstructor
    val fieldConstT = typeOf[FieldConst[_, _]].typeConstructor
    val fieldComputedT = typeOf[FieldComputed[_, _]].typeConstructor

    if (overridesTpe == emptyT) {
      config
    } else if (overridesTpe.typeConstructor == disableDefaultsT) {
      captureConfiguration(overridesTpe.typeArgs.head, config.copy(disableDefaultValues = true))
    } else if (overridesTpe.typeConstructor == fieldConstT) {
      val List(fieldNameT, rest) = overridesTpe.typeArgs
      val fieldName =
        fieldNameT.asInstanceOf[scala.reflect.internal.Types#UniqueConstantType].value.value.asInstanceOf[String]
      captureConfiguration(rest, config.copy(overridenFields = config.overridenFields + fieldName))
    } else if (overridesTpe.typeConstructor == fieldComputedT) {
      val List(fieldNameT, rest) = overridesTpe.typeArgs
      val fieldName =
        fieldNameT.asInstanceOf[scala.reflect.internal.Types#UniqueConstantType].value.value.asInstanceOf[String]
      captureConfiguration(rest, config.copy(overridenFields = config.overridenFields + fieldName))
    } else {
      c.abort(c.enclosingPosition, "Bad overriden type shape!")
    }
  }
}
