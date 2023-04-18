package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.ImplicitTransformerPreference
import io.scalaland.chimney.internal
import io.scalaland.chimney.partial
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.TransformerCfg

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Configurations { this: Definitions =>

  final protected case class TransformerFlags(
      processDefaultValues: Boolean = false,
      beanSetters: Boolean = false,
      beanGetters: Boolean = false,
      methodAccessors: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      implicitConflictResolution: Option[ImplicitTransformerPreference] = None
  ) {

    def setBoolFlag[Flag <: internal.TransformerFlags.Flag: Type](value: Boolean): TransformerFlags =
      if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.DefaultValues) {
        copy(processDefaultValues = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.BeanSetters) {
        copy(beanSetters = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.BeanGetters) {
        copy(beanGetters = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.MethodAccessors) {
        copy(methodAccessors = value)
      } else if (Type[Flag] =:= ChimneyType.TransformerFlags.Flags.OptionDefaultsToNone) {
        copy(optionDefaultsToNone = value)
      } else {
        // $COVERAGE-OFF$
        reportError(s"Invalid transformer flag type: ${Type[Flag]}!")
        // $COVERAGE-ON$
      }

    def setImplicitConflictResolution(preference: Option[ImplicitTransformerPreference]): TransformerFlags =
      copy(implicitConflictResolution = preference)
  }

  protected case class FieldOverride[From, ToField](
      toFieldName: String,
      toFieldType: Type[ToField],
      exprSource: FieldOverride.ValueSource[From, ToField]
  )
  protected object FieldOverride {

    def fromRuntimeConfiguration[From: Type, ToField: Type](
        toFieldName: String,
        runtimeConfiguration: FieldOverride.RuntimeConfiguration,
        runtimeDataStore: Expr[dsls.TransformerDefinitionCommons.RuntimeDataStore]
    ): FieldOverride[From, ToField] =
      FieldOverride(
        toFieldName,
        Type[ToField],
        configurationsImpl.extractRuntimeConfiguration[From, ToField](runtimeConfiguration, runtimeDataStore)
      )

    sealed trait ValueSource[From, ToField] extends Product with Serializable
    object ValueSource {
      final case class Const[From, ToField](
          value: Expr[ToField]
      ) extends ValueSource[From, ToField]

      final case class ConstPartial[From, ToField](
          value: Expr[partial.Result[ToField]]
      ) extends ValueSource[From, ToField]

      final case class Computed[From, ToField](
          compute: Expr[From] => Expr[ToField]
      ) extends ValueSource[From, ToField]

      final case class ComputedPartial[From, ToField](
          compute: Expr[From] => Expr[partial.Result[ToField]]
      ) extends ValueSource[From, ToField]

      final case class Renamed[From, FromField, ToField](
          fromFieldName: String,
          fromFieldType: Type[FromField],
          originalValue: Expr[From] => Expr[FromField]
      ) extends ValueSource[From, ToField]
    }

    sealed abstract class RuntimeConfiguration(val needValueLevelAccess: Boolean) extends Product with Serializable
    object RuntimeConfiguration {

      final case class Const(runtimeDataIdx: Int) extends RuntimeConfiguration(true)
      final case class ConstPartial(runtimeDataIdx: Int) extends RuntimeConfiguration(true)
      final case class Computed(runtimeDataIdx: Int) extends RuntimeConfiguration(true)
      final case class ComputedPartial(runtimeDataIdx: Int) extends RuntimeConfiguration(true)
      final case class RenamedFrom(sourceName: String) extends RuntimeConfiguration(false)
    }
  }

  sealed protected trait CoproductOverride[From, FromSubtype <: From, To] extends Product with Serializable {
    val fromSubtype: Type[FromSubtype]
  }
  protected object CoproductOverride {

    final case class CoproductTotalInstance[From, FromSubtype <: From, To](
        fromSubtype: Type[FromSubtype],
        convert: Expr[FromSubtype] => Expr[To]
    ) extends CoproductOverride[From, FromSubtype, To]

    final case class CoproductPartialInstance[From, FromSubtype <: From, To](
        fromSubtype: Type[FromSubtype],
        convert: Expr[FromSubtype] => Expr[partial.Result[To]]
    ) extends CoproductOverride[From, FromSubtype, To]
  }

  final protected case class TransformerConfig[From, To](
      flags: TransformerFlags = TransformerFlags(),
      fieldOverrides: Map[String, FieldOverride[From, ?]] = Map.empty,
      coproductOverride: Vector[CoproductOverride[From, ? <: From, To]] = Vector.empty,
      preventResolutionForTypes: Option[(ComputedType, ComputedType)] = None,
      legacy: TransformerConfig.LegacyData = TransformerConfig.LegacyData() // TODO: temporary
  )
  object TransformerConfig {

    type UpdateCfg[_ <: TransformerCfg]

    // TODO: for creating TransformerConfig for old macros in Scala 2 until everything is migrated
    final case class LegacyData(
        fieldOverrideLegacy: Map[String, FieldOverride.RuntimeConfiguration] = Map.empty,
        coproductInstanceOverridesLegacy: Map[(ComputedType, ComputedType), Int] = Map.empty,
        coproductInstancesPartialOverridesLegacy: Map[(ComputedType, ComputedType), Int] = Map.empty,
        transformerDefinitionPrefix: Expr[dsls.TransformerDefinitionCommons[UpdateCfg]] =
          null.asInstanceOf[Expr[dsls.TransformerDefinitionCommons[UpdateCfg]]],
        definitionScope: Option[(ComputedType, ComputedType)] = None
    )
  }

  protected def configurationsImpl: ConfigurationDefinitionsImpl
  protected trait ConfigurationDefinitionsImpl {

    def extractRuntimeConfiguration[From: Type, ToField: Type](
        runtimeConfiguration: FieldOverride.RuntimeConfiguration,
        runtimeDataStore: Expr[dsls.TransformerDefinitionCommons.RuntimeDataStore]
    ): FieldOverride.ValueSource[From, ToField]

    def readTransformerConfig[
        From: Type,
        To: Type,
        Cfg <: internal.TransformerCfg: Type,
        InstanceFlags <: internal.TransformerFlags: Type,
        SharedFlags <: internal.TransformerFlags: Type
    ]: TransformerConfig[From, To]
  }
}
