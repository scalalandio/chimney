package io.scalaland.chimney.internal

import scala.reflect.macros.whitebox

trait DslMacros {
  this: TransformerMacros with DslMacros with MacroUtils with DerivationConfig =>

  val c: whitebox.Context

  import c.universe._

  def expandWithFieldConst[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag](fieldName: Name,
                                                                                     value: c.Tree): c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]

    val fieldNameConst = Constant(fieldName.decodedName.toString)
    val fieldNameLit = Literal(fieldNameConst)
    val singletonFieldTpe = c.internal.constantType(fieldNameConst)
    val newCfgTpe = tq"_root_.io.scalaland.chimney.internal.FieldConst[$singletonFieldTpe, $C]"
    val fn = TermName(c.freshName("ti"))

    q"""
      {
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.dsl.TransformerInto[$From, $To, $newCfgTpe]($fn.source, $fn.overrides.updated($fieldNameLit, $value))
      }
    """
  }

  def expandFieldComputed[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag](fieldName: Name,
                                                                                    map: c.Tree): c.Tree = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]

    val fieldNameConst = Constant(fieldName.decodedName.toString)
    val fieldNameLit = Literal(fieldNameConst)
    val singletonFieldTpe = c.internal.constantType(fieldNameConst)
    val newCfgTpe = tq"_root_.io.scalaland.chimney.internal.FieldComputed[$singletonFieldTpe, $C]"
    val fn = TermName(c.freshName("ti"))

    q"""
      {
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.dsl.TransformerInto[$From, $To, $newCfgTpe]($fn.source, $fn.overrides.updated($fieldNameLit, $map($fn.source)))
      }
    """
  }

  def expandFieldRenamed[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag](fieldNameFrom: Name,
                                                                                   fieldNameTo: Name): c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]

    val singletonFromTpe = c.internal.constantType(Constant(fieldNameFrom.decodedName.toString))
    val singletonToTpe = c.internal.constantType(Constant(fieldNameTo.decodedName.toString))

    val newCfgTpe = tq"_root_.io.scalaland.chimney.internal.FieldRelabelled[$singletonFromTpe, $singletonToTpe, $C]"

    q"${c.prefix.tree}.asInstanceOf[_root_.io.scalaland.chimney.dsl.TransformerInto[$From, $To, $newCfgTpe]]"
  }

  def expandTansformInto[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Tree = {
    val config = Config()
    val derivedTransformerTree = genTransformer[From, To](config).tree
    q"$derivedTransformerTree.transform(${c.prefix.tree}.source)".debug
  }

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
