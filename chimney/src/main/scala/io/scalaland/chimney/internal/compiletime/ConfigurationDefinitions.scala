package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.ImplicitTransformerPreference
import io.scalaland.chimney.internal

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait ConfigurationDefinitions { this: Definitions =>

  sealed abstract protected class FieldOverride(val needValueLevelAccess: Boolean)
      extends Product
      with Serializable
  protected object FieldOverride {
    final case class Const(runtimeDataIdx: Int) extends FieldOverride(true)
    final case class ConstPartial(runtimeDataIdx: Int) extends FieldOverride(true)
    final case class Computed(runtimeDataIdx: Int) extends FieldOverride(true)
    final case class ComputedPartial(runtimeDataIdx: Int) extends FieldOverride(true)
    final case class RenamedFrom(sourceName: String) extends FieldOverride(false)
  }

  sealed protected trait DerivationTarget extends Product with Serializable {
    def targetType[To: Type]: ComputedType
    def isPartial: Boolean
  }
  protected object DerivationTarget {
    // derivation target instance of `Transformer[A, B]`
    case object TotalTransformer extends DerivationTarget {
      override def targetType[To: Type]: ComputedType = ComputedType(Type[To])

      // $COVERAGE-OFF$
      override def isPartial = false
      // $COVERAGE-ON$
    }

    // derivation target instance of `PartialTransformer[A, B]`
    final case class PartialTransformer(failFastExpr: Expr[Boolean]) extends DerivationTarget {
      override def targetType[To: Type]: ComputedType = ComputedType(Type.PartialResult[To])

      // $COVERAGE-OFF$
      override def isPartial = true
      // $COVERAGE-ON$
    }
  }

  final protected case class TransformerFlags(
      processDefaultValues: Boolean = false,
      beanSetters: Boolean = false,
      beanGetters: Boolean = false,
      methodAccessors: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      implicitConflictResolution: Option[ImplicitTransformerPreference] = None
  ) {

    def setBoolFlag[Flag <: internal.TransformerFlags.Flag: Type](value: Boolean): TransformerFlags = {
      if (Type[Flag] =:= Type.TransformerFlags.Flags.DefaultValues) {
        copy(processDefaultValues = value)
      } else if (Type[Flag] =:= Type.TransformerFlags.Flags.BeanSetters) {
        copy(beanSetters = value)
      } else if (Type[Flag] =:= Type.TransformerFlags.Flags.BeanGetters) {
        copy(beanGetters = value)
      } else if (Type[Flag] =:= Type.TransformerFlags.Flags.MethodAccessors) {
        copy(methodAccessors = value)
      } else if (Type[Flag] =:= Type.TransformerFlags.Flags.OptionDefaultsToNone) {
        copy(optionDefaultsToNone = value)
      } else {
        // $COVERAGE-OFF$
        // TODO
        /// c.abort(c.enclosingPosition, s"Invalid transformer flag type: $flagTpe!")
        ???
        // $COVERAGE-ON$
      }
    }

    def setImplicitConflictResolution(preference: Option[ImplicitTransformerPreference]): TransformerFlags = {
      copy(implicitConflictResolution = preference)
    }
  }

  final protected case class TransformerConfig( // TODO: rename to TransformerContext
      // srcPrefixTree: Tree = EmptyTree,
      derivationTarget: DerivationTarget = DerivationTarget.TotalTransformer,
      flags: TransformerFlags = TransformerFlags(),
      fieldOverrides: Map[String, FieldOverride] = Map.empty,
      coproductInstanceOverrides: Map[TransformerConfig.CoproductInstanceOverrideTotal, Int] = Map.empty,
      coproductInstancesPartialOverrides: Map[TransformerConfig.CoproductInstanceOverridePartial, Int] = Map.empty,
      // transformerDefinitionPrefix: Tree = EmptyTree,
      definitionScope: Option[TransformerConfig.DefinitionScope] = None
  ) {

    import TransformerConfig.*

    // def withSrcPrefixTree(srcPrefixTree: Tree): TransformerConfig =
    //  copy(srcPrefixTree = srcPrefixTree)

    def withDerivationTarget(derivationTarget: DerivationTarget): TransformerConfig =
      copy(derivationTarget = derivationTarget)

    // def withTransformerDefinitionPrefix(tdPrefix: Tree): TransformerConfig =
    //  copy(transformerDefinitionPrefix = tdPrefix)

    def withDefinitionScope[From: Type, To: Type]: TransformerConfig =
      copy(definitionScope = Some(DefinitionScope(ComputedType(Type[From]), ComputedType(Type[To]))))

    def rec: TransformerConfig =
      copy(
        definitionScope = None,
        fieldOverrides = Map.empty
      )

    def valueLevelAccessNeeded: Boolean =
      fieldOverrides.exists { case (_, fo) => fo.needValueLevelAccess } ||
        coproductInstanceOverrides.nonEmpty ||
        coproductInstancesPartialOverrides.nonEmpty

    def fieldOverride(fieldName: String, fieldOverride: FieldOverride): TransformerConfig =
      copy(fieldOverrides = fieldOverrides + (fieldName -> fieldOverride))

    def coproductInstance[From: Type, To: Type](runtimeDataIdx: Int): TransformerConfig =
      copy(coproductInstanceOverrides =
        coproductInstanceOverrides + (CoproductInstanceOverrideTotal(
          ComputedType(Type[From]),
          ComputedType(Type[To])
        ) -> runtimeDataIdx)
      )

    def coproductInstancePartial[From: Type, To: Type](runtimeDataIdx: Int): TransformerConfig =
      copy(coproductInstancesPartialOverrides =
        coproductInstancesPartialOverrides + (CoproductInstanceOverridePartial(
          ComputedType(Type[From]),
          ComputedType(Type[To])
        ) -> runtimeDataIdx)
      )
  }
  protected object TransformerConfig {
    final case class CoproductInstanceOverrideTotal(from: ComputedType, to: ComputedType)
    final case class CoproductInstanceOverridePartial(from: ComputedType, to: ComputedType)
    final case class DefinitionScope(from: ComputedType, to: ComputedType)
  }

  protected def readConfig[
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: TransformerConfig
}
