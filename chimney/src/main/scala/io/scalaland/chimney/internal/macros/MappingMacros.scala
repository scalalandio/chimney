package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.{DerivationError, IncompatibleSourceTuple}
import io.scalaland.chimney.internal.utils.{DerivationGuards, MacroUtils}

import scala.collection.immutable.ListMap
import scala.reflect.macros.blackbox

trait MappingMacros extends Model with TransformerConfigSupport {
  this: DerivationGuards with MacroUtils =>

  val c: blackbox.Context

  import c.universe._

  def resolveSourceTupleAccessors(
      From: Type,
      To: Type
  ): Either[Seq[DerivationError], Map[Target, AccessorResolution.Resolved]] = {
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
            .headOption
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
      config.fieldOverrides.get(target.name) match {
        case Some(FieldOverride.Const) =>
          Some {
            target -> TransformerBodyTree(
              config.transformerDefinitionPrefix.accessConst(target.name, target.tpe),
              isWrapped = false
            )
          }
        case Some(FieldOverride.ConstF) if config.wrapperType.isDefined =>
          val fTargetTpe = config.wrapperType.get.applyTypeArg(target.tpe)
          Some {
            target -> TransformerBodyTree(
              config.transformerDefinitionPrefix.accessConst(target.name, fTargetTpe),
              isWrapped = true
            )
          }
        case Some(FieldOverride.Computed) =>
          Some {
            target -> TransformerBodyTree(
              config.transformerDefinitionPrefix.accessComputed(target.name, srcPrefixTree, From, target.tpe),
              isWrapped = false
            )
          }
        case Some(FieldOverride.ComputedF) if config.wrapperType.isDefined =>
          val fTargetTpe = config.wrapperType.get.applyTypeArg(target.tpe)
          Some {
            target -> TransformerBodyTree(
              config.transformerDefinitionPrefix.accessComputed(target.name, srcPrefixTree, From, fTargetTpe),
              isWrapped = true
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
            .map(defaultValueTree => target -> TransformerBodyTree(defaultValueTree, isWrapped = false))
        } else {
          None
        }

      def optionNoneFallback =
        if (config.flags.optionDefaultsToNone && isOption(target.tpe)) {
          Some(target -> TransformerBodyTree(q"_root_.scala.None", isWrapped = false))
        } else {
          None
        }

      def unitFallback =
        if (isUnit(target.tpe)) {
          Some(target -> TransformerBodyTree(q"()", isWrapped = false))
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
