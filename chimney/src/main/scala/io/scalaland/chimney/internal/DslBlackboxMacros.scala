package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait DslBlackboxMacros {
  this: TransformerMacros with MacroUtils with DerivationConfig =>

  val c: blackbox.Context

  import c.universe._

  def buildDefinedTransformer[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Tree = {
    val C = weakTypeOf[C]
    val tdName = TermName(c.freshName("td"))
    val config = captureConfiguration(C).copy(
      transformerDefinitionPrefix = q"$tdName",
      definitionScope = Some((weakTypeOf[From], weakTypeOf[To]))
    )

    val derivedTransformerTree = genTransformer[From, To](config).tree

    q"""
       val $tdName = ${c.prefix.tree}
       $derivedTransformerTree
    """
  }

  def expandTransform[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Tree = {
    val C = weakTypeOf[C]
    val tiName = TermName(c.freshName("ti"))
    val config = captureConfiguration(C).copy(transformerDefinitionPrefix = q"$tiName.td")

    val derivedTransformerTree = genTransformer[From, To](config).tree

    q"""
       val $tiName = ${c.prefix.tree}
       $derivedTransformerTree.transform($tiName.source)
    """
  }

  def captureConfiguration(cfgTpe: Type, config: Config = Config()): Config = {

    val emptyT = typeOf[Empty]
    val disableDefaultValuesT = typeOf[DisableDefaultValues[_]].typeConstructor
    val enableBeanGettersT = typeOf[EnableBeanGetters[_]].typeConstructor
    val enableBeanSettersT = typeOf[EnableBeanSetters[_]].typeConstructor
    val enableOptionDefaultsToNone = typeOf[EnableOptionDefaultsToNone[_]].typeConstructor
    val enableUnsafeOption = typeOf[EnableUnsafeOption[_]].typeConstructor
    val fieldConstT = typeOf[FieldConst[_, _]].typeConstructor
    val fieldComputedT = typeOf[FieldComputed[_, _]].typeConstructor
    val fieldRelabelledT = typeOf[FieldRelabelled[_, _, _]].typeConstructor
    val coproductInstanceT = typeOf[CoproductInstance[_, _, _]].typeConstructor

    if (cfgTpe == emptyT) {
      config
    } else if (cfgTpe.typeConstructor == disableDefaultValuesT) {
      captureConfiguration(cfgTpe.typeArgs.head, config.copy(processDefaultValues = false))
    } else if (cfgTpe.typeConstructor == enableBeanGettersT) {
      captureConfiguration(cfgTpe.typeArgs.head, config.copy(enableBeanGetters = true))
    } else if (cfgTpe.typeConstructor == enableBeanSettersT) {
      captureConfiguration(cfgTpe.typeArgs.head, config.copy(enableBeanSetters = true))
    } else if (cfgTpe.typeConstructor == enableOptionDefaultsToNone) {
      captureConfiguration(cfgTpe.typeArgs.head, config.copy(optionDefaultsToNone = true))
    } else if (cfgTpe.typeConstructor == enableUnsafeOption) {
      captureConfiguration(cfgTpe.typeArgs.head, config.copy(enableUnsafeOption = true))
    } else if (cfgTpe.typeConstructor == fieldConstT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureConfiguration(rest, config.copy(
        constFields = config.constFields + fieldName,
        computedFields = config.computedFields - fieldName
      ))
    } else if (cfgTpe.typeConstructor == fieldComputedT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureConfiguration(rest, config.copy(
        computedFields = config.computedFields + fieldName,
        constFields = config.constFields - fieldName
      ))
    } else if (cfgTpe.typeConstructor == fieldRelabelledT) {
      val List(fieldNameFromT, fieldNameToT, rest) = cfgTpe.typeArgs
      val fieldNameFrom = fieldNameFromT.singletonString
      val fieldNameTo = fieldNameToT.singletonString
      captureConfiguration(rest, config.copy(renamedFields = config.renamedFields.updated(fieldNameTo, fieldNameFrom)))
    } else if (cfgTpe.typeConstructor == coproductInstanceT) {
      val List(instanceType, targetType, rest) = cfgTpe.typeArgs
      captureConfiguration(
        rest,
        config.copy(coproductInstances = config.coproductInstances + (instanceType.typeSymbol -> targetType))
      )
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal config type shape!")
      // $COVERAGE-ON$
    }
  }
}
