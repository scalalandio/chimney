package io.scalaland.chimney.internal

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.language.existentials
import scala.reflect.macros.blackbox

sealed abstract class TransformerCfg

object TransformerCfg {
  final class Empty extends TransformerCfg
  final class DisableDefaultValues[C <: TransformerCfg] extends TransformerCfg
  final class EnableBeanGetters[C <: TransformerCfg] extends TransformerCfg
  final class EnableBeanSetters[C <: TransformerCfg] extends TransformerCfg
  final class EnableOptionDefaultsToNone[C <: TransformerCfg] extends TransformerCfg
  final class EnableUnsafeOption[C <: TransformerCfg] extends TransformerCfg
  final class EnableMethodAccessors[C <: TransformerCfg] extends TransformerCfg
  final class EnableOptCollectionFlattening[C <: TransformerCfg] extends TransformerCfg
  final class FieldConst[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldConstF[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldComputed[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldComputedF[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldRelabelled[FromName <: String, ToName <: String, C <: TransformerCfg] extends TransformerCfg
  final class CoproductInstance[InstType, TargetType, C <: TransformerCfg] extends TransformerCfg
  final class CoproductInstanceF[InstType, TargetType, C <: TransformerCfg] extends TransformerCfg
  final class WrapperType[F[+_], C <: TransformerCfg] extends TransformerCfg
}

trait TransformerConfiguration extends MacroUtils {

  val c: blackbox.Context

  import c.universe._

  sealed abstract class FieldOverride(val needValueLevelAccess: Boolean)

  object FieldOverride {
    case object Const extends FieldOverride(true)
    case object ConstF extends FieldOverride(true)
    case object Computed extends FieldOverride(true)
    case object ComputedF extends FieldOverride(true)
    case class RenamedFrom(sourceName: String) extends FieldOverride(false)
  }

  case class TransformerConfig(
      processDefaultValues: Boolean = true,
      enableBeanGetters: Boolean = false,
      enableBeanSetters: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      enableUnsafeOption: Boolean = false,
      enableMethodAccessors: Boolean = false,
      enableOptCollectionFlattening: Boolean = false,
      fieldOverrides: Map[String, FieldOverride] = Map.empty,
      coproductInstances: Set[(Symbol, Type)] = Set.empty, // pair: inst type, target type
      transformerDefinitionPrefix: Tree = EmptyTree,
      definitionScope: Option[(Type, Type)] = None,
      wrapperType: Option[Type] = None,
      wrapperSupportInstance: Tree = EmptyTree,
      coproductInstancesF: Set[(Symbol, Type)] = Set.empty // pair: inst type, target type,
  ) {

    def rec: TransformerConfig =
      copy(
        definitionScope = None,
        fieldOverrides = Map.empty
      )

    def valueLevelAccessNeeded: Boolean = {
      fieldOverrides.exists { case (_, fo) => fo.needValueLevelAccess } ||
      coproductInstances.nonEmpty ||
      coproductInstancesF.nonEmpty
    }

    def fieldOverride(fieldName: String, fieldOverride: FieldOverride): TransformerConfig = {
      copy(fieldOverrides = fieldOverrides + (fieldName -> fieldOverride))
    }

    def coproductInstance(instanceType: Type, targetType: Type): TransformerConfig = {
      copy(coproductInstances = coproductInstances + (instanceType.typeSymbol -> targetType))
    }

    def coproductInstanceF(instanceType: Type, targetType: Type): TransformerConfig = {
      copy(coproductInstancesF = coproductInstancesF + (instanceType.typeSymbol -> targetType))
    }
  }

  object CfgTpeConstructors {
    import TransformerCfg._

    val emptyT = typeOf[Empty]
    val disableDefaultValuesT = typeOf[DisableDefaultValues[_]].typeConstructor
    val enableBeanGettersT = typeOf[EnableBeanGetters[_]].typeConstructor
    val enableBeanSettersT = typeOf[EnableBeanSetters[_]].typeConstructor
    val enableOptionDefaultsToNone = typeOf[EnableOptionDefaultsToNone[_]].typeConstructor
    val enableUnsafeOption = typeOf[EnableUnsafeOption[_]].typeConstructor
    val enableMethodAccessors = typeOf[EnableMethodAccessors[_]].typeConstructor
    val enableOptCollectionFlattening = typeOf[EnableOptCollectionFlattening[_]].typeConstructor
    val fieldConstT = typeOf[FieldConst[_, _]].typeConstructor
    val fieldConstFT = typeOf[FieldConstF[_, _]].typeConstructor
    val fieldComputedT = typeOf[FieldComputed[_, _]].typeConstructor
    val fieldComputedFT = typeOf[FieldComputedF[_, _]].typeConstructor
    val fieldRelabelledT = typeOf[FieldRelabelled[_, _, _]].typeConstructor
    val coproductInstanceT = typeOf[CoproductInstance[_, _, _]].typeConstructor
    val coproductInstanceFT = typeOf[CoproductInstanceF[_, _, _]].typeConstructor
    val wrapperTypeT = typeOf[WrapperType[F, _] forSome { type F[+_] }].typeConstructor
  }

  def captureTransformerConfig(cfgTpe: Type): TransformerConfig = {

    import CfgTpeConstructors._

    if (cfgTpe =:= emptyT) {
      TransformerConfig()
    } else if (cfgTpe.typeConstructor =:= disableDefaultValuesT) {
      captureTransformerConfig(cfgTpe.typeArgs.head).copy(processDefaultValues = false)
    } else if (cfgTpe.typeConstructor =:= enableBeanGettersT) {
      captureTransformerConfig(cfgTpe.typeArgs.head).copy(enableBeanGetters = true)
    } else if (cfgTpe.typeConstructor =:= enableBeanSettersT) {
      captureTransformerConfig(cfgTpe.typeArgs.head).copy(enableBeanSetters = true)
    } else if (cfgTpe.typeConstructor =:= enableOptionDefaultsToNone) {
      captureTransformerConfig(cfgTpe.typeArgs.head).copy(optionDefaultsToNone = true)
    } else if (cfgTpe.typeConstructor =:= enableUnsafeOption) {
      captureTransformerConfig(cfgTpe.typeArgs.head).copy(enableUnsafeOption = true)
    } else if (cfgTpe.typeConstructor =:= enableMethodAccessors) {
      captureTransformerConfig(cfgTpe.typeArgs.head).copy(enableMethodAccessors = true)
    } else if (cfgTpe.typeConstructor =:= enableOptCollectionFlattening) {
      captureTransformerConfig(cfgTpe.typeArgs.head).copy(enableOptCollectionFlattening = true)
    } else if (cfgTpe.typeConstructor =:= fieldConstT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureTransformerConfig(rest).fieldOverride(fieldName, FieldOverride.Const)
    } else if (cfgTpe.typeConstructor =:= fieldComputedT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureTransformerConfig(rest).fieldOverride(fieldName, FieldOverride.Computed)
    } else if (cfgTpe.typeConstructor =:= fieldRelabelledT) {
      val List(fieldNameFromT, fieldNameToT, rest) = cfgTpe.typeArgs
      val fieldNameFrom = fieldNameFromT.singletonString
      val fieldNameTo = fieldNameToT.singletonString
      captureTransformerConfig(rest)
        .fieldOverride(fieldNameTo, FieldOverride.RenamedFrom(fieldNameFrom))
    } else if (cfgTpe.typeConstructor =:= coproductInstanceT) {
      val List(instanceType, targetType, rest) = cfgTpe.typeArgs
      captureTransformerConfig(rest).coproductInstance(instanceType, targetType)
    } else if (cfgTpe.typeConstructor =:= wrapperTypeT) {
      val List(f, rest) = cfgTpe.typeArgs
      captureTransformerConfig(rest).copy(wrapperType = Some(f))
    } else if (cfgTpe.typeConstructor =:= fieldConstFT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureTransformerConfig(rest).fieldOverride(fieldName, FieldOverride.ConstF)
    } else if (cfgTpe.typeConstructor =:= fieldComputedFT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureTransformerConfig(rest).fieldOverride(fieldName, FieldOverride.ComputedF)
    } else if (cfgTpe.typeConstructor =:= coproductInstanceFT) {
      val List(instanceType, targetType, rest) = cfgTpe.typeArgs
      captureTransformerConfig(rest).coproductInstanceF(instanceType, targetType)
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal transformer config type shape!")
      // $COVERAGE-ON$
    }
  }
}
