package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait DslBlackboxMacros {
  this: TransformerMacros with MacroUtils with DerivationConfig =>

  val c: blackbox.Context

  import c.universe._

  def expandTansform[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Tree = {
    val C = weakTypeOf[C]
    val srcName = c.freshName("src")
    val config = captureConfiguration(C).copy(prefixValName = srcName)

    val derivedTransformerTree = genTransformer[From, To](config).tree

    q"""
       val ${TermName(srcName)} = ${c.prefix.tree}
       $derivedTransformerTree.transform(${TermName(srcName)}.source)
    """
  }

  def captureConfiguration(overridesTpe: Type, config: Config = Config()): Config = {

    val emptyT = typeOf[Empty]
    val disableDefaultsT = typeOf[DisableDefaults[_]].typeConstructor
    val fieldConstT = typeOf[FieldConst[_, _]].typeConstructor
    val fieldComputedT = typeOf[FieldComputed[_, _]].typeConstructor
    val fieldRelabelledT = typeOf[FieldRelabelled[_, _, _]].typeConstructor

    if (overridesTpe == emptyT) {
      config
    } else if (overridesTpe.typeConstructor == disableDefaultsT) {
      captureConfiguration(overridesTpe.typeArgs.head, config.copy(disableDefaultValues = true))
    } else if (Set(fieldConstT, fieldComputedT).contains(overridesTpe.typeConstructor)) {
      val List(fieldNameT, rest) = overridesTpe.typeArgs
      val fieldNameConst = fieldNameT.asInstanceOf[scala.reflect.internal.Types#UniqueConstantType].value
      val fieldName = fieldNameConst.value.asInstanceOf[String]
      captureConfiguration(rest, config.copy(overridenFields = config.overridenFields + fieldName))
    } else if (overridesTpe.typeConstructor == fieldRelabelledT) {
      val List(fieldNameFromT, fieldNameToT, rest) = overridesTpe.typeArgs
      val fieldNameFromConst = fieldNameFromT.asInstanceOf[scala.reflect.internal.Types#UniqueConstantType].value
      val fieldNameFrom = fieldNameFromConst.value.asInstanceOf[String]
      val fieldNameToConst = fieldNameToT.asInstanceOf[scala.reflect.internal.Types#UniqueConstantType].value
      val fieldNameTo = fieldNameToConst.value.asInstanceOf[String]
      captureConfiguration(rest, config.copy(renamedFields = config.renamedFields.updated(fieldNameTo, fieldNameFrom)))
    } else {
      c.abort(c.enclosingPosition, "Bad overriden type shape!")
    }
  }
}
