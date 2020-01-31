package io.scalaland.chimney.internal

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.blackbox

sealed abstract class TransformerCfg

object TransformerCfg {
  final class Empty extends TransformerCfg
  final class DisableDefaultValues[C <: TransformerCfg] extends TransformerCfg
  final class EnableBeanGetters[C <: TransformerCfg] extends TransformerCfg
  final class EnableBeanSetters[C <: TransformerCfg] extends TransformerCfg
  final class EnableOptionDefaultsToNone[C <: TransformerCfg] extends TransformerCfg
  final class EnableUnsafeOption[C <: TransformerCfg] extends TransformerCfg
  final class FieldConst[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldComputed[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldRelabelled[FromName <: String, ToName <: String, C <: TransformerCfg] extends TransformerCfg
  final class CoproductInstance[InstType, TargetType, C <: TransformerCfg] extends TransformerCfg
}

trait TransformerConfiguration extends MacroUtils {

  val c: blackbox.Context

  import c.universe._

  case class TransformerConfig(
      processDefaultValues: Boolean = true,
      enableBeanGetters: Boolean = false,
      enableBeanSetters: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      enableUnsafeOption: Boolean = false,
      constFields: Set[String] = Set.empty,
      computedFields: Set[String] = Set.empty,
      renamedFields: Map[String, String] = Map.empty,
      coproductInstances: Set[(c.Symbol, c.Type)] = Set.empty, // pair: inst type, target type
      transformerDefinitionPrefix: c.Tree = c.universe.EmptyTree,
      definitionScope: Option[(c.Type, c.Type)] = None
  ) {

    def rec: TransformerConfig =
      copy(
        constFields = Set.empty,
        computedFields = Set.empty,
        renamedFields = Map.empty,
        definitionScope = None
      )

    def valueLevelAccessNeeded: Boolean = {
      constFields.nonEmpty || computedFields.nonEmpty || coproductInstances.nonEmpty
    }
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
}
