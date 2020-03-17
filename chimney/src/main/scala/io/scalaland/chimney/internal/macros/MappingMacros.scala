package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.{DerivationError, IncompatibleSourceTuple, TransformerConfiguration}
import io.scalaland.chimney.internal.utils.{DerivationGuards, MacroUtils}

import scala.reflect.macros.blackbox

trait MappingMacros extends Model with TransformerConfiguration {
  this: DerivationGuards with MacroUtils =>

  val c: blackbox.Context

  import c.universe._

  def resolveSourceTupleAccessors(
      From: Type,
      To: Type
  ): Either[Seq[DerivationError], Map[Target, ResolvedAccessor]] = {
    val tupleElems = From.caseClassParams
    val targetFields = To.caseClassParams

    if (tupleElems.size != targetFields.size) {
      Left {
        Seq(
          IncompatibleSourceTuple(
            tupleElems.size,
            targetFields.size,
            From.typeSymbol.fullName.toString,
            To.typeSymbol.fullName.toString
          )
        )
      }
    } else {
      Right {
        (tupleElems zip targetFields).map {
          case (tupleElem, targetField) =>
            Target.fromField(targetField, To) -> ResolvedAccessor(tupleElem, wasRenamed = false)
        }.toMap
      }
    }
  }

  def resolveAccessorsMapping(
      From: Type,
      targets: Iterable[Target],
      config: TransformerConfig
  ): Map[Target, Option[ResolvedAccessor]] = {
    val fromGetters = From.getterMethods
    targets.map { target =>
      target -> {
        val lookupName = config.renamedFields.getOrElse(target.name, target.name)
        val wasRenamed = lookupName != target.name
        fromGetters
          .find(lookupAccessor(config, lookupName, wasRenamed, From))
          .map(ms => ResolvedAccessor(ms, wasRenamed))
      }
    }.toMap
  }

  def resolveOverrides(
      srcPrefixTree: Tree,
      From: Type,
      targets: Iterable[Target],
      config: TransformerConfig
  ): Map[Target, TransformerBodyTree] = {
    targets.flatMap { target =>
      if (config.wrapperType.isDefined && config.constFFields.contains(target.name)) {
        Some {
          target -> TransformerBodyTree(
            q"""
             ${config.transformerDefinitionPrefix}
               .overrides(${target.name})
               .asInstanceOf[${config.wrapperType.get.applyTypeArg(target.tpe)}]
            """,
            isWrapped = true
          )
        }
      } else if (config.wrapperType.isDefined && config.computedFFields.contains(target.name)) {
        val fTargetTpe = config.wrapperType.get.applyTypeArg(target.tpe)
        Some {
          target -> TransformerBodyTree(
            q"""
             ${config.transformerDefinitionPrefix}
               .overrides(${target.name})
               .asInstanceOf[$From => $fTargetTpe]
               .apply($srcPrefixTree)
               .asInstanceOf[$fTargetTpe]
            """,
            isWrapped = true
          )
        }
      } else if (config.constFields.contains(target.name)) {
        Some {
          target -> TransformerBodyTree(
            q"""
             ${config.transformerDefinitionPrefix}
                .overrides(${target.name})
                .asInstanceOf[${target.tpe}]
            """,
            isWrapped = false
          )
        }
      } else if (config.computedFields.contains(target.name)) {
        Some {
          target -> TransformerBodyTree(
            q"""
             ${config.transformerDefinitionPrefix}
                .overrides(${target.name})
                .asInstanceOf[$From => ${target.tpe}]
                .apply($srcPrefixTree)
                .asInstanceOf[${target.tpe}]
            """,
            isWrapped = false
          )
        }
      } else {
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

    targets.flatMap { target =>
      def defaultValueFallback =
        if (config.processDefaultValues && To.isCaseClass) {
          targetCaseClassDefaults
            .get(target.name)
            .map(defaultValueTree => target -> TransformerBodyTree(defaultValueTree, isWrapped = false))
        } else {
          None
        }

      def optionNoneFallback =
        if (config.optionDefaultsToNone && isOption(target.tpe)) {
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
    }.toMap
  }

  def lookupAccessor(
      config: TransformerConfig,
      lookupName: String,
      wasRenamed: Boolean,
      From: Type
  )(ms: MethodSymbol): Boolean = {
    val sourceName = ms.name.decodedName.toString
    if (config.enableBeanGetters) {
      val lookupNameCapitalized = lookupName.capitalize
      sourceName == lookupName ||
      sourceName == s"get$lookupNameCapitalized" ||
      (sourceName == s"is$lookupNameCapitalized" && ms.resultTypeIn(From) == typeOf[Boolean])
    } else {
      (ms.isStable || wasRenamed || config.enableMethodAccessors) && // isStable means or val/lazy val
      sourceName == lookupName
    }
  }

}
