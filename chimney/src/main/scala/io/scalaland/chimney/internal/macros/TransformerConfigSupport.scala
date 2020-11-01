package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.language.existentials
import scala.reflect.macros.blackbox

trait TransformerConfigSupport extends MacroUtils {

  val c: blackbox.Context

  import c.universe._

  def readConfig[C: WeakTypeTag, InstanceFlags: WeakTypeTag, ScopeFlags: WeakTypeTag](
      wrapperSupportInstance: Tree
  ): TransformerConfig = {
    val C = weakTypeOf[C]
    val InstanceFlags = weakTypeOf[InstanceFlags]
    val ScopeFlags = weakTypeOf[ScopeFlags]

    val scopeFlags = captureTransformerFlags(ScopeFlags)
    val combinedFlags = captureTransformerFlags(InstanceFlags, scopeFlags)

    captureTransformerConfig(C).copy(
      flags = combinedFlags,
      wrapperSupportInstance = wrapperSupportInstance
    )
  }

  sealed abstract class FieldOverride(val needValueLevelAccess: Boolean)

  object FieldOverride {
    case object Const extends FieldOverride(true)
    case object ConstF extends FieldOverride(true)
    case object Computed extends FieldOverride(true)
    case object ComputedF extends FieldOverride(true)
    case class RenamedFrom(sourceName: String) extends FieldOverride(false)
  }

  case class TransformerConfig(
      flags: TransformerFlags = TransformerFlags(),
      fieldOverrides: Map[String, FieldOverride] = Map.empty,
      coproductInstances: Set[(Symbol, Type)] = Set.empty, // pair: inst type, target type
      transformerDefinitionPrefix: Tree = EmptyTree,
      definitionScope: Option[(Type, Type)] = None,
      wrapperType: Option[Type] = None,
      wrapperSupportInstance: Tree = EmptyTree,
      wrapperErrorPathSupportInstance: Option[Tree] = None,
      coproductInstancesF: Set[(Symbol, Type)] = Set.empty // pair: inst type, target type
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

  object CfgTpes {

    import io.scalaland.chimney.internal.TransformerCfg._

    val emptyT: Type = typeOf[Empty]
    val fieldConstT: Type = typeOf[FieldConst[_, _]].typeConstructor
    val fieldConstFT: Type = typeOf[FieldConstF[_, _]].typeConstructor
    val fieldComputedT: Type = typeOf[FieldComputed[_, _]].typeConstructor
    val fieldComputedFT: Type = typeOf[FieldComputedF[_, _]].typeConstructor
    val fieldRelabelledT: Type = typeOf[FieldRelabelled[_, _, _]].typeConstructor
    val coproductInstanceT: Type = typeOf[CoproductInstance[_, _, _]].typeConstructor
    val coproductInstanceFT: Type = typeOf[CoproductInstanceF[_, _, _]].typeConstructor
    val wrapperTypeT: Type = typeOf[WrapperType[F, _] forSome { type F[+_] }].typeConstructor
  }

  def captureTransformerConfig(rawCfgTpe: Type): TransformerConfig = {

    import CfgTpes._

    val cfgTpe = rawCfgTpe.dealias

    if (cfgTpe =:= emptyT) {
      TransformerConfig()
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

  case class TransformerFlags(
      methodAccessors: Boolean = false,
      processDefaultValues: Boolean = true,
      beanSetters: Boolean = false,
      beanGetters: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      unsafeOption: Boolean = false
  ) {
    def setFlag(flagTpe: Type, value: Boolean): TransformerFlags = {
      if (flagTpe =:= FlagsTpes.methodAccessorsT) {
        copy(methodAccessors = value)
      } else if (flagTpe =:= FlagsTpes.defaultValuesT) {
        copy(processDefaultValues = value)
      } else if (flagTpe =:= FlagsTpes.beanSettersT) {
        copy(beanSetters = value)
      } else if (flagTpe =:= FlagsTpes.beanGettersT) {
        copy(beanGetters = value)
      } else if (flagTpe =:= FlagsTpes.optionDefaultsToNoneT) {
        copy(optionDefaultsToNone = value)
      } else if (flagTpe =:= FlagsTpes.unsafeOptionT) {
        copy(unsafeOption = value)
      } else {
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, s"Invalid transformer flag type: $flagTpe!")
        // $COVERAGE-ON$
      }
    }
  }

  object FlagsTpes {

    import io.scalaland.chimney.internal.TransformerFlags._

    val defaultT: Type = typeOf[Default]
    val enableT: Type = typeOf[Enable[_, _]].typeConstructor
    val disableT: Type = typeOf[Disable[_, _]].typeConstructor

    val methodAccessorsT: Type = typeOf[MethodAccessors]
    val defaultValuesT: Type = typeOf[DefaultValues]
    val beanSettersT: Type = typeOf[BeanSetters]
    val beanGettersT: Type = typeOf[BeanGetters]
    val optionDefaultsToNoneT: Type = typeOf[OptionDefaultsToNone]
    val unsafeOptionT: Type = typeOf[UnsafeOption]
  }

  def captureTransformerFlags(
      rawFlagsTpe: Type,
      defaultFlags: TransformerFlags = TransformerFlags()
  ): TransformerFlags = {

    import FlagsTpes._

    val flagsTpe = rawFlagsTpe.dealias

    if (flagsTpe =:= defaultT) {
      defaultFlags
    } else if (flagsTpe.typeConstructor =:= enableT) {
      val List(flagT, rest) = flagsTpe.typeArgs
      captureTransformerFlags(rest, defaultFlags).setFlag(flagT, value = true)
    } else if (flagsTpe.typeConstructor =:= disableT) {
      val List(flagT, rest) = flagsTpe.typeArgs
      captureTransformerFlags(rest, defaultFlags).setFlag(flagT, value = false)
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal transformer flags type shape!")
      // $COVERAGE-ON$
    }
  }

  def captureFromTransformerConfigurationTree(transformerConfigurationTree: Tree): TransformerFlags = {
    transformerConfigurationTree.tpe.typeArgs.headOption
      .map(flagsTpe => captureTransformerFlags(flagsTpe))
      .getOrElse(TransformerFlags())
  }

}
