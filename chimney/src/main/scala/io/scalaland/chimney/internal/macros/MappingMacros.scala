package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.{TypeTestUtils, DslMacroUtils}
import io.scalaland.chimney.internal.{TransformerDerivationError, IncompatibleSourceTuple}

import scala.collection.immutable.ListMap
import scala.reflect.macros.blackbox

trait MappingMacros extends Model with TypeTestUtils with DslMacroUtils {

  val c: blackbox.Context

  import c.universe._

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
        (tupleElems zip targetFields).map {
          case (tupleElem, targetField) =>
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

    ListMap(accessorsMapping.toSeq: _*)
  }

  def resolveOverrides(
      srcPrefixTree: Tree,
      From: Type,
      targets: Iterable[Target],
      config: TransformerConfig
  ): Map[Target, TransformerBodyTree] = {
    targets.flatMap { target =>
      config.fieldOverrides.get(target.name).flatMap {

        case FieldOverride.Const =>
          Some {
            target -> TransformerBodyTree(
              config.transformerDefinitionPrefix.accessOverriddenConstValue(target.name, target.tpe),
              DerivationTarget.TotalTransformer
            )
          }

        case FieldOverride.ConstPartial if config.derivationTarget.isPartial =>
          val fTargetTpe = config.derivationTarget.targetType(target.tpe)
          Some {
            target -> TransformerBodyTree(
              q"""
                ${config.transformerDefinitionPrefix.accessOverriddenConstValue(target.name, fTargetTpe)}
                  .prependErrorPath(
                    _root_.io.scalaland.chimney.PartialTransformer.PathElement.Accessor(${target.name})
                  )
              """,
              config.derivationTarget
            )
          }

        case FieldOverride.Computed if config.derivationTarget.isPartial =>
          val function =
            config.transformerDefinitionPrefix.accessOverriddenComputedFunction(target.name, From, target.tpe)
          val liftedFunction = q"_root_.io.scalaland.chimney.PartialTransformer.Result.fromFunction($function)"
          Some {
            target -> TransformerBodyTree(
              q"""
                ${liftedFunction.callUnaryApply(srcPrefixTree)}
                  .prependErrorPath(
                    _root_.io.scalaland.chimney.PartialTransformer.PathElement.Accessor(${target.name})
                  )
              """,
              config.derivationTarget
            )
          }

        case FieldOverride.Computed =>
          Some {
            target -> TransformerBodyTree(
              config.transformerDefinitionPrefix
                .accessOverriddenComputedFunction(target.name, From, target.tpe)
                .callUnaryApply(srcPrefixTree),
              DerivationTarget.TotalTransformer
            )
          }

        case FieldOverride.ComputedPartial if config.derivationTarget.isPartial =>
          val fTargetTpe = config.derivationTarget.targetType(target.tpe)
          Some {
            target -> TransformerBodyTree(
              q"""
                ${config.transformerDefinitionPrefix
                .accessOverriddenComputedFunction(target.name, From, fTargetTpe)
                .callUnaryApply(srcPrefixTree)}
                  .prependErrorPath(
                    _root_.io.scalaland.chimney.PartialTransformer.PathElement.Accessor(${target.name})
                  )
              """,
              config.derivationTarget
            )
          }

        // following cases (lifted ConstF/ComputedF) to be removed soon

        case FieldOverride.ConstF if config.derivationTarget.isLifted =>
          val fTargetTpe = config.derivationTarget.targetType(target.tpe)
          Some {
            target -> TransformerBodyTree(
              config.transformerDefinitionPrefix.accessOverriddenConstValue(target.name, fTargetTpe),
              config.derivationTarget
            )
          }

        case FieldOverride.ComputedF if config.derivationTarget.isLifted =>
          val fTargetTpe = config.derivationTarget.targetType(target.tpe)
          Some {
            target -> TransformerBodyTree(
              config.transformerDefinitionPrefix
                .accessOverriddenComputedFunction(target.name, From, fTargetTpe)
                .callUnaryApply(srcPrefixTree),
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
  ): Map[Target, TransformerBodyTree] = {

    lazy val targetCaseClassDefaults = To.typeSymbol.asClass.caseClassDefaults

    val fallbackTransformers = targets.flatMap { target =>
      def defaultValueFallback =
        if (config.flags.processDefaultValues && To.isCaseClass) {
          targetCaseClassDefaults
            .get(target.name)
            .map(defaultValueTree => target -> TransformerBodyTree(defaultValueTree, DerivationTarget.TotalTransformer))
        } else {
          None
        }

      def optionNoneFallback =
        if (config.flags.optionDefaultsToNone && isOption(target.tpe)) {
          Some(target -> TransformerBodyTree(q"_root_.scala.None", DerivationTarget.TotalTransformer))
        } else {
          None
        }

      def unitFallback =
        if (isUnit(target.tpe)) {
          Some(target -> TransformerBodyTree(q"()", DerivationTarget.TotalTransformer))
        } else {
          None
        }

      defaultValueFallback orElse optionNoneFallback orElse unitFallback
    }
    ListMap(fallbackTransformers.toSeq: _*)
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
      if (sourceName == lookupName ||
          sourceName == s"get$lookupNameCapitalized" ||
          (sourceName == s"is$lookupNameCapitalized" && ms.resultTypeIn(From) == typeOf[Boolean])) {
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
