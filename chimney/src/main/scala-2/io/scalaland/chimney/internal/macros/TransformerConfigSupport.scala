package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, PreferPartialTransformer, PreferTotalTransformer}
import io.scalaland.chimney.partial
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.blackbox

trait TransformerConfigSupport extends MacroUtils {

  val c: blackbox.Context

  import c.universe.*

  def readConfig[C: WeakTypeTag, InstanceFlags: WeakTypeTag, ImplicitScopeFlags: WeakTypeTag]: TransformerConfig = {
    val implicitScopeFlags = captureTransformerFlags(weakTypeOf[ImplicitScopeFlags])
    val combinedFlags = captureTransformerFlags(weakTypeOf[InstanceFlags], implicitScopeFlags)

    captureTransformerConfig(weakTypeOf[C], runtimeDataIdx = 0).copy(flags = combinedFlags)
  }

  sealed abstract class FieldOverride(val needValueLevelAccess: Boolean)

  object FieldOverride {
    case class Const(runtimeDataIdx: Int) extends FieldOverride(true)
    case class ConstPartial(runtimeDataIdx: Int) extends FieldOverride(true)
    case class Computed(runtimeDataIdx: Int) extends FieldOverride(true)
    case class ComputedPartial(runtimeDataIdx: Int) extends FieldOverride(true)
    case class RenamedFrom(sourceName: String) extends FieldOverride(false)
  }

  sealed trait DerivationTarget {
    def targetType(toTpe: Type): Type
    def isPartial: Boolean
  }

  object DerivationTarget {
    // derivation target instance of `Transformer[A, B]`
    case object TotalTransformer extends DerivationTarget {
      def targetType(toTpe: Type): Type = toTpe
      // $COVERAGE-OFF$
      def isPartial = false
      // $COVERAGE-ON$
    }
    // derivation target instance of `PartialTransformer[A, B]`
    case class PartialTransformer(failFastTermName: TermName = freshTermName("failFast")) extends DerivationTarget {
      def failFastTree: Tree = q"$failFastTermName"
      def targetType(toTpe: Type): Type =
        typeOf[partial.Result[?]].typeConstructor.applyTypeArg(toTpe)
      // $COVERAGE-OFF$
      def isPartial = true
      // $COVERAGE-ON$
    }
  }

  case class TransformerConfig( // TODO: rename to TransformerContext
      srcPrefixTree: Tree = EmptyTree,
      derivationTarget: DerivationTarget = DerivationTarget.TotalTransformer,
      flags: TransformerFlags = TransformerFlags(),
      fieldOverrides: Map[String, FieldOverride] = Map.empty,
      coproductInstanceOverrides: Map[(Symbol, Type), Int] = Map.empty,
      coproductInstancesPartialOverrides: Map[(Symbol, Type), Int] = Map.empty,
      transformerDefinitionPrefix: Tree = EmptyTree,
      definitionScope: Option[(Type, Type)] = None
  ) {

    def withSrcPrefixTree(srcPrefixTree: Tree): TransformerConfig =
      copy(srcPrefixTree = srcPrefixTree)
    def withDerivationTarget(derivationTarget: DerivationTarget): TransformerConfig =
      copy(derivationTarget = derivationTarget)
    def withTransformerDefinitionPrefix(tdPrefix: Tree): TransformerConfig =
      copy(transformerDefinitionPrefix = tdPrefix)
    def withDefinitionScope(fromTpe: Type, toTpe: Type): TransformerConfig =
      copy(definitionScope = Some((fromTpe, toTpe)))

    def rec: TransformerConfig =
      copy(
        definitionScope = None,
        fieldOverrides = Map.empty
      )

    def valueLevelAccessNeeded: Boolean = {
      fieldOverrides.exists { case (_, fo) => fo.needValueLevelAccess } ||
      coproductInstanceOverrides.nonEmpty ||
      coproductInstancesPartialOverrides.nonEmpty
    }

    def fieldOverride(fieldName: String, fieldOverride: FieldOverride): TransformerConfig = {
      copy(fieldOverrides = fieldOverrides + (fieldName -> fieldOverride))
    }

    def coproductInstance(instanceType: Type, targetType: Type, runtimeDataIdx: Int): TransformerConfig = {
      copy(coproductInstanceOverrides =
        coproductInstanceOverrides + ((instanceType.typeSymbol, targetType) -> runtimeDataIdx)
      )
    }

    def coproductInstancePartial(instanceType: Type, targetType: Type, runtimeDataIdx: Int): TransformerConfig = {
      copy(coproductInstancesPartialOverrides =
        coproductInstancesPartialOverrides + ((
          instanceType.typeSymbol,
          targetType
        ) -> runtimeDataIdx)
      )
    }
  }

  object CfgTpes {

    import io.scalaland.chimney.internal.TransformerCfg.*

    // We cannot get typeOf[HigherKind] directly, but we can get the typeOf[ExistentialType]
    // and extract type constructor out of it.

    val emptyT: Type = typeOf[Empty]
    val fieldConstT: Type = typeOf[FieldConst[?, ?]].typeConstructor
    val fieldConstPartialT: Type = typeOf[FieldConstPartial[?, ?]].typeConstructor
    val fieldComputedT: Type = typeOf[FieldComputed[?, ?]].typeConstructor
    val fieldComputedPartialT: Type = typeOf[FieldComputedPartial[?, ?]].typeConstructor
    val fieldRelabelledT: Type = typeOf[FieldRelabelled[?, ?, ?]].typeConstructor
    val coproductInstanceT: Type = typeOf[CoproductInstance[?, ?, ?]].typeConstructor
    val coproductInstancePartialT: Type = typeOf[CoproductInstancePartial[?, ?, ?]].typeConstructor
  }

  private def captureTransformerConfig(rawCfgTpe: Type, runtimeDataIdx: Int): TransformerConfig = {

    import CfgTpes.*

    val cfgTpe = rawCfgTpe.dealias

    if (cfgTpe =:= emptyT) {
      TransformerConfig()
    } else if (cfgTpe.typeConstructor =:= fieldConstT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureTransformerConfig(rest, 1 + runtimeDataIdx)
        .fieldOverride(fieldName, FieldOverride.Const(runtimeDataIdx))
    } else if (cfgTpe.typeConstructor =:= fieldComputedT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureTransformerConfig(rest, 1 + runtimeDataIdx)
        .fieldOverride(fieldName, FieldOverride.Computed(runtimeDataIdx))
    } else if (cfgTpe.typeConstructor =:= fieldRelabelledT) {
      val List(fieldNameFromT, fieldNameToT, rest) = cfgTpe.typeArgs
      val fieldNameFrom = fieldNameFromT.singletonString
      val fieldNameTo = fieldNameToT.singletonString
      captureTransformerConfig(rest, runtimeDataIdx)
        .fieldOverride(fieldNameTo, FieldOverride.RenamedFrom(fieldNameFrom))
    } else if (cfgTpe.typeConstructor =:= coproductInstanceT) {
      val List(instanceType, targetType, rest) = cfgTpe.typeArgs
      captureTransformerConfig(rest, 1 + runtimeDataIdx)
        .coproductInstance(instanceType, targetType, runtimeDataIdx)
    } else if (cfgTpe.typeConstructor =:= fieldConstPartialT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureTransformerConfig(rest, 1 + runtimeDataIdx)
        .fieldOverride(fieldName, FieldOverride.ConstPartial(runtimeDataIdx))
    } else if (cfgTpe.typeConstructor =:= fieldComputedPartialT) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureTransformerConfig(rest, 1 + runtimeDataIdx)
        .fieldOverride(fieldName, FieldOverride.ComputedPartial(runtimeDataIdx))
    } else if (cfgTpe.typeConstructor =:= coproductInstancePartialT) {
      val List(instanceType, targetType, rest) = cfgTpe.typeArgs
      captureTransformerConfig(rest, 1 + runtimeDataIdx)
        .coproductInstancePartial(instanceType, targetType, runtimeDataIdx)
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal transformer config type shape!")
      // $COVERAGE-ON$
    }
  }

  case class TransformerFlags(
      methodAccessors: Boolean = false,
      processDefaultValues: Boolean = false,
      beanSetters: Boolean = false,
      beanGetters: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      implicitConflictResolution: Option[ImplicitTransformerPreference] = None
  ) {
    def setBoolFlag(flagTpe: Type, value: Boolean): TransformerFlags = {
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
      } else {
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, s"Invalid transformer flag type: $flagTpe!")
        // $COVERAGE-ON$
      }
    }

    def setImplicitConflictResolution(preference: Option[ImplicitTransformerPreference]): TransformerFlags = {
      copy(implicitConflictResolution = preference)
    }
  }

  object FlagsTpes {

    import io.scalaland.chimney.internal.TransformerFlags.*

    val defaultT: Type = typeOf[Default]
    val enableT: Type = typeOf[Enable[?, ?]].typeConstructor
    val disableT: Type = typeOf[Disable[?, ?]].typeConstructor

    val methodAccessorsT: Type = typeOf[MethodAccessors]
    val defaultValuesT: Type = typeOf[DefaultValues]
    val beanSettersT: Type = typeOf[BeanSetters]
    val beanGettersT: Type = typeOf[BeanGetters]
    val optionDefaultsToNoneT: Type = typeOf[OptionDefaultsToNone]
    val implicitConflictResolutionT: Type = typeOf[ImplicitConflictResolution[?]].typeConstructor
  }

  def captureTransformerFlags(
      rawFlagsTpe: Type,
      defaultFlags: TransformerFlags = TransformerFlags()
  ): TransformerFlags = {

    import FlagsTpes.*

    val flagsTpe = rawFlagsTpe.dealias

    if (flagsTpe =:= defaultT) {
      defaultFlags
    } else if (flagsTpe.typeConstructor =:= enableT) {
      val List(flagT, rest) = flagsTpe.typeArgs

      if (flagT.typeConstructor =:= implicitConflictResolutionT) {
        val preferenceT = flagT.typeArgs.head
        if (preferenceT =:= typeOf[PreferTotalTransformer.type]) {
          captureTransformerFlags(rest, defaultFlags).setImplicitConflictResolution(Some(PreferTotalTransformer))
        } else if (preferenceT =:= typeOf[PreferPartialTransformer.type]) {
          captureTransformerFlags(rest, defaultFlags).setImplicitConflictResolution(Some(PreferPartialTransformer))
        } else {
          // $COVERAGE-OFF$
          c.abort(c.enclosingPosition, "Invalid implicit conflict resolution preference type!!")
          // $COVERAGE-ON$
        }
      } else {
        captureTransformerFlags(rest, defaultFlags).setBoolFlag(flagT, value = true)
      }
    } else if (flagsTpe.typeConstructor =:= disableT) {
      val List(flagT, rest) = flagsTpe.typeArgs

      if (flagT.typeConstructor =:= implicitConflictResolutionT) {
        captureTransformerFlags(rest, defaultFlags).setImplicitConflictResolution(None)
      } else {
        captureTransformerFlags(rest, defaultFlags).setBoolFlag(flagT, value = false)
      }
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal transformer flags type shape!")
      // $COVERAGE-ON$
    }
  }

  def captureFromTransformerConfigurationTree(transformerConfigurationTree: Tree): TransformerFlags = {
    transformerConfigurationTree.tpe.typeArgs.headOption
      .map(flagsTpe => captureTransformerFlags(flagsTpe))
      .getOrElse {
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, "Impossible case: TransformerConfiguration without type parameter!")
        // $COVERAGE-ON$
      }
  }

}
