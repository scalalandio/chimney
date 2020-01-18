package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.blackbox

trait DslBlackboxMacros {
  this: TransformerMacros with MacroUtils with DerivationConfig =>

  val c: blackbox.Context

  import c.universe._

  def buildDefinedTransformer[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Tree = {
    val C = weakTypeOf[C]
    val tdName = TermName(c.freshName("td"))
    val config = captureTransformerConfig(C).copy(
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
    val config = captureTransformerConfig(C).copy(transformerDefinitionPrefix = q"$tiName.td")

    val derivedTransformerTree = genTransformer[From, To](config).tree

    q"""
       val $tiName = ${c.prefix.tree}
       $derivedTransformerTree.transform($tiName.source)
    """
  }

  def captureTransformerConfig(cfgTpe: Type, config: TransformerConfig = TransformerConfig()): TransformerConfig = {

    import TransformerCfg._

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

    if (cfgTpe =:= emptyT) {
      config
    } else if (cfgTpe.typeConstructor =:= disableDefaultValuesT) {
      captureTransformerConfig(cfgTpe.typeArgs.head, config.copy(processDefaultValues = false))
    } else if (cfgTpe.typeConstructor =:= enableBeanGettersT) {
      captureTransformerConfig(cfgTpe.typeArgs.head, config.copy(enableBeanGetters = true))
    } else if (cfgTpe.typeConstructor =:= enableBeanSettersT) {
      captureTransformerConfig(cfgTpe.typeArgs.head, config.copy(enableBeanSetters = true))
    } else if (cfgTpe.typeConstructor =:= enableOptionDefaultsToNone) {
      captureTransformerConfig(cfgTpe.typeArgs.head, config.copy(optionDefaultsToNone = true))
    } else if (cfgTpe.typeConstructor =:= enableUnsafeOption) {
      captureTransformerConfig(cfgTpe.typeArgs.head, config.copy(enableUnsafeOption = true))
    } else if (cfgTpe.typeConstructor =:= fieldConstT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureTransformerConfig(
        rest,
        config.copy(
          constFields = config.constFields + fieldName,
          computedFields = config.computedFields - fieldName
        )
      )
    } else if (cfgTpe.typeConstructor =:= fieldComputedT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureTransformerConfig(
        rest,
        config.copy(
          computedFields = config.computedFields + fieldName,
          constFields = config.constFields - fieldName
        )
      )
    } else if (cfgTpe.typeConstructor =:= fieldRelabelledT) {
      val List(fieldNameFromT, fieldNameToT, rest) = cfgTpe.typeArgs
      val fieldNameFrom = fieldNameFromT.singletonString
      val fieldNameTo = fieldNameToT.singletonString
      captureTransformerConfig(
        rest,
        config.copy(renamedFields = config.renamedFields.updated(fieldNameTo, fieldNameFrom))
      )
    } else if (cfgTpe.typeConstructor =:= coproductInstanceT) {
      val List(instanceType, targetType, rest) = cfgTpe.typeArgs
      captureTransformerConfig(
        rest,
        config.copy(coproductInstances = config.coproductInstances + (instanceType.typeSymbol -> targetType))
      )
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal transformer config type shape!")
      // $COVERAGE-ON$
    }
  }

  def capturePatcherConfig(cfgTpe: Type, config: PatcherConfig = PatcherConfig()): PatcherConfig = {

    import PatcherCfg._

    val emptyT = typeOf[Empty]
    val enableIncompletePatches = typeOf[EnableIncompletePatches[_]].typeConstructor

    if (cfgTpe =:= emptyT) {
      config
    } else if (cfgTpe.typeConstructor =:= enableIncompletePatches) {
      capturePatcherConfig(cfgTpe.typeArgs.head, config.copy(enableIncompletePatches = true))
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal patcher config type shape!")
      // $COVERAGE-ON$
    }
  }
}
