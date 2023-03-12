package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.{DslMacroUtils, TypeTestUtils}
import io.scalaland.chimney.internal.{IncompatibleSourceTuple, TransformerDerivationError}

import scala.collection.immutable.ListMap
import scala.reflect.macros.blackbox

trait MappingMacros extends Model with TypeTestUtils with DslMacroUtils with GenTrees {

  val c: blackbox.Context

  import c.universe.*

  def resolveSourceTupleAccessors(
      From: Type,
      To: Type
  ): Either[Seq[TransformerDerivationError], Map[Target, AccessorResolution.Resolved]] = {
    val tupleElems = From.caseClassParams
    val targetFields = To.caseClassParams

    if (tupleElems.size != targetFields.size) {
      Left {
        Seq(
          IncompatibleSourceTuple(
            tupleElems.size,
            targetFields.size,
            From.typeSymbol.fullName,
            To.typeSymbol.fullName
          )
        )
      }
    } else {
      Right {
        (tupleElems zip targetFields).map { case (tupleElem, targetField) =>
          Target.fromField(targetField, To) -> AccessorResolution.Resolved(tupleElem, wasRenamed = false)
        }.toMap
      }
    }
  }

  def resolveAccessorsMapping(
      From: Type,
      targets: Iterable[Target],
      config: TransformerConfig
  ): Map[Target, AccessorResolution] = {
    val fromGetters = From.getterMethods
    val accessorsMapping = targets
      .map { target =>
        target -> {
          val lookupName = config.fieldOverrides.get(target.name) match {
            case Some(FieldOverride.RenamedFrom(sourceName)) => sourceName
            case _                                           => target.name
          }
          val wasRenamed = lookupName != target.name
          fromGetters
            .map(lookupAccessor(config, lookupName, wasRenamed, From))
            .find(_ != AccessorResolution.NotFound)
            .getOrElse(AccessorResolution.NotFound)
        }
      }

    ListMap(accessorsMapping.toSeq*)
  }

  def resolveOverrides(
      From: Type,
      targets: Iterable[Target],
      config: TransformerConfig
  ): Map[Target, DerivedTree] = {
    targets.flatMap { target =>
      config.fieldOverrides.get(target.name) match {

        case Some(FieldOverride.Const(runtimeDataIdx)) =>
          Some {
            target -> DerivedTree(
              config.transformerDefinitionPrefix.accessOverriddenConstValue(runtimeDataIdx, target.tpe),
              DerivationTarget.TotalTransformer
            )
          }

        case Some(FieldOverride.ConstPartial(runtimeDataIdx)) if config.derivationTarget.isPartial =>
          val fTargetTpe = config.derivationTarget.targetType(target.tpe)
          Some {
            target -> DerivedTree(
              q"""
                ${config.transformerDefinitionPrefix.accessOverriddenConstValue(runtimeDataIdx, fTargetTpe)}
                  .prependErrorPath(${Trees.PathElement.accessor(target.name)})
              """,
              config.derivationTarget
            )
          }

        case Some(FieldOverride.Computed(runtimeDataIdx)) if config.derivationTarget.isPartial =>
          val function =
            config.transformerDefinitionPrefix.accessOverriddenComputedFunction(runtimeDataIdx, From, target.tpe)
          val liftedFunction = Trees.PartialResult.fromFunction(function)
          Some {
            target -> DerivedTree(
              q"""
                ${liftedFunction.callUnaryApply(config.srcPrefixTree)}
                  .prependErrorPath(${Trees.PathElement.accessor(target.name)})
              """,
              config.derivationTarget
            )
          }

        case Some(FieldOverride.Computed(runtimeDataIdx)) =>
          Some {
            target -> DerivedTree(
              config.transformerDefinitionPrefix
                .accessOverriddenComputedFunction(runtimeDataIdx, From, target.tpe)
                .callUnaryApply(config.srcPrefixTree),
              DerivationTarget.TotalTransformer
            )
          }

        case Some(FieldOverride.ComputedPartial(runtimeDataIdx)) if config.derivationTarget.isPartial =>
          val fTargetTpe = config.derivationTarget.targetType(target.tpe)
          Some {
            target -> DerivedTree(
              q"""
                ${config.transformerDefinitionPrefix
                  .accessOverriddenComputedFunction(runtimeDataIdx, From, fTargetTpe)
                  .callUnaryApply(config.srcPrefixTree)}
                  .prependErrorPath(${Trees.PathElement.accessor(target.name)})
              """,
              config.derivationTarget
            )
          }

        // following cases (lifted ConstF/ComputedF) to be removed soon

        case Some(FieldOverride.ConstF(runtimeDataIdx)) if config.derivationTarget.isLifted =>
          val fTargetTpe = config.derivationTarget.targetType(target.tpe)
          Some {
            target -> DerivedTree(
              config.transformerDefinitionPrefix.accessOverriddenConstValue(runtimeDataIdx, fTargetTpe),
              config.derivationTarget
            )
          }

        case Some(FieldOverride.ComputedF(runtimeDataIdx)) if config.derivationTarget.isLifted =>
          val fTargetTpe = config.derivationTarget.targetType(target.tpe)
          Some {
            target -> DerivedTree(
              config.transformerDefinitionPrefix
                .accessOverriddenComputedFunction(runtimeDataIdx, From, fTargetTpe)
                .callUnaryApply(config.srcPrefixTree),
              config.derivationTarget
            )
          }

        case _ =>
          None
      }
    }.toMap
  }

  def resolveFallbackTransformerBodies(
      targets: Iterable[Target],
      To: Type,
      config: TransformerConfig
  ): Map[Target, DerivedTree] = {

    lazy val targetCaseClassDefaults = To.typeSymbol.asClass.caseClassDefaults

    val fallbackTransformers = targets.flatMap { target =>
      def defaultValueFallback =
        if (config.flags.processDefaultValues && To.isCaseClass) {
          targetCaseClassDefaults
            .get(target.name)
            .map(defaultValueTree => target -> DerivedTree(defaultValueTree, DerivationTarget.TotalTransformer))
        } else {
          None
        }

      def optionNoneFallback =
        if (config.flags.optionDefaultsToNone && isOption(target.tpe)) {
          Some(target -> DerivedTree(Trees.Option.none, DerivationTarget.TotalTransformer))
        } else {
          None
        }

      def unitFallback =
        if (isUnit(target.tpe)) {
          Some(target -> DerivedTree(Trees.unit, DerivationTarget.TotalTransformer))
        } else {
          None
        }

      defaultValueFallback orElse optionNoneFallback orElse unitFallback
    }
    ListMap(fallbackTransformers.toSeq*)
  }

  def lookupAccessor(
      config: TransformerConfig,
      lookupName: String,
      wasRenamed: Boolean,
      From: Type
  )(ms: MethodSymbol): AccessorResolution = {
    val sourceName = ms.name.decodedName.toString
    if (config.flags.beanGetters) {
      val lookupNameCapitalized = lookupName.capitalize
      if (
        sourceName == lookupName ||
        sourceName == s"get$lookupNameCapitalized" ||
        (sourceName == s"is$lookupNameCapitalized" && ms.resultTypeIn(From) == typeOf[Boolean])
      ) {
        AccessorResolution.Resolved(ms, wasRenamed = false)
      } else {
        AccessorResolution.NotFound
      }
    } else {
      if (sourceName == lookupName) {
        if (ms.isStable || wasRenamed || config.flags.methodAccessors) { // isStable means or val/lazy val
          AccessorResolution.Resolved(ms, wasRenamed)
        } else {
          AccessorResolution.DefAvailable
        }
      } else {
        AccessorResolution.NotFound
      }
    }
  }

}
