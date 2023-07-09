package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

private[compiletime] trait ChimneyTypes { this: ChimneyDefinitions =>

  protected val ChimneyType: ChimneyTypeModule
  protected trait ChimneyTypeModule {

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]]

    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]]

    def Patcher[A: Type, Patch: Type]: Type[Patcher[A, Patch]]

    val PartialResult: PartialResultModule
    trait PartialResultModule extends Type.Ctor1[partial.Result] { this: PartialResult.type =>
      def Value[A: Type]: Type[partial.Result.Value[A]]
      val Errors: Type[partial.Result.Errors]
    }

    val PathElement: PathElementModule
    trait PathElementModule { this: PathElement.type =>
      val tpe: Type[partial.PathElement]
      val Accessor: Type[partial.PathElement.Accessor]
      val Index: Type[partial.PathElement.Index]
      val MapKey: Type[partial.PathElement.MapKey]
      val MapValue: Type[partial.PathElement.MapValue]
    }

    val PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    val PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    val TransformerCfg: TransformerCfgModule
    trait TransformerCfgModule {
      val Empty: Type[runtime.TransformerCfg.Empty]

      val FieldConst: FieldConstModule
      trait FieldConstModule
          extends Type.Ctor2UpperBounded[
            String,
            runtime.TransformerCfg,
            runtime.TransformerCfg.FieldConst
          ] { this: FieldConst.type => }

      val FieldConstPartial: FieldConstPartialModule
      trait FieldConstPartialModule
          extends Type.Ctor2UpperBounded[
            String,
            runtime.TransformerCfg,
            runtime.TransformerCfg.FieldConstPartial
          ] { this: FieldConstPartial.type => }

      val FieldComputed: FieldComputedModule
      trait FieldComputedModule
          extends Type.Ctor2UpperBounded[
            String,
            runtime.TransformerCfg,
            runtime.TransformerCfg.FieldComputed
          ] { this: FieldComputed.type => }

      val FieldComputedPartial: FieldComputedPartialModule
      trait FieldComputedPartialModule
          extends Type.Ctor2UpperBounded[
            String,
            runtime.TransformerCfg,
            runtime.TransformerCfg.FieldComputedPartial
          ] { this: FieldComputedPartial.type => }

      val FieldRelabelled: FieldRelabelledModule
      trait FieldRelabelledModule
          extends Type.Ctor3UpperBounded[
            String,
            String,
            runtime.TransformerCfg,
            runtime.TransformerCfg.FieldRelabelled
          ] { this: FieldRelabelled.type => }

      val CoproductInstance: CoproductInstanceModule
      trait CoproductInstanceModule
          extends Type.Ctor3UpperBounded[
            Any,
            Any,
            runtime.TransformerCfg,
            runtime.TransformerCfg.CoproductInstance
          ] { this: CoproductInstance.type => }

      val CoproductInstancePartial: CoproductInstancePartialModule
      trait CoproductInstancePartialModule
          extends Type.Ctor3UpperBounded[
            Any,
            Any,
            runtime.TransformerCfg,
            runtime.TransformerCfg.CoproductInstancePartial
          ] { this: CoproductInstancePartial.type => }
    }

    val TransformerFlags: TransformerFlagsModule
    trait TransformerFlagsModule { this: TransformerFlags.type =>
      val Default: Type[runtime.TransformerFlags.Default]

      val Enable: EnableModule
      trait EnableModule
          extends Type.Ctor2UpperBounded[
            runtime.TransformerFlags.Flag,
            runtime.TransformerFlags,
            runtime.TransformerFlags.Enable
          ] { this: Enable.type => }

      val Disable: DisableModule
      trait DisableModule
          extends Type.Ctor2UpperBounded[
            runtime.TransformerFlags.Flag,
            runtime.TransformerFlags,
            runtime.TransformerFlags.Disable
          ] { this: Disable.type => }

      val Flags: FlagsModule
      trait FlagsModule { this: Flags.type =>
        val DefaultValues: Type[runtime.TransformerFlags.DefaultValues]
        val BeanGetters: Type[runtime.TransformerFlags.BeanGetters]
        val BeanSetters: Type[runtime.TransformerFlags.BeanSetters]
        val MethodAccessors: Type[runtime.TransformerFlags.MethodAccessors]
        val OptionDefaultsToNone: Type[runtime.TransformerFlags.OptionDefaultsToNone]
        val ImplicitConflictResolution: ImplicitConflictResolutionModule
        trait ImplicitConflictResolutionModule
            extends Type.Ctor1UpperBounded[
              dsls.ImplicitTransformerPreference,
              runtime.TransformerFlags.ImplicitConflictResolution
            ] { this: ImplicitConflictResolution.type => }
        val MacrosLogging: Type[runtime.TransformerFlags.MacrosLogging]
      }
    }

    val PatcherCfg: PatcherCfgModule
    trait PatcherCfgModule {
      val Empty: Type[runtime.PatcherCfg.Empty]

      val IgnoreRedundantPatcherFields: IgnoreRedundantPatcherFieldsModule
      trait IgnoreRedundantPatcherFieldsModule
          extends Type.Ctor1UpperBounded[
            runtime.PatcherCfg,
            runtime.PatcherCfg.IgnoreRedundantPatcherFields
          ] { this: IgnoreRedundantPatcherFields.type => }

      val IgnoreNoneInPatch: IgnoreNoneInPatchModule
      trait IgnoreNoneInPatchModule
          extends Type.Ctor1UpperBounded[
            runtime.PatcherCfg,
            runtime.PatcherCfg.IgnoreNoneInPatch
          ] { this: IgnoreNoneInPatch.type => }

      val MacrosLogging: MacrosLoggingModule
      trait MacrosLoggingModule
          extends Type.Ctor1UpperBounded[
            runtime.PatcherCfg,
            runtime.PatcherCfg.MacrosLogging
          ] { this: MacrosLogging.type => }
    }

    val TransformerInto: TransformerIntoModule
    trait TransformerIntoModule
        extends Type.Ctor4UpperBounded[
          Any,
          Any,
          runtime.TransformerCfg,
          runtime.TransformerFlags,
          dsls.TransformerInto
        ] { this: TransformerInto.type => }

    val TransformerDefinition: TransformerDefinitionModule
    trait TransformerDefinitionModule
        extends Type.Ctor4UpperBounded[
          Any,
          Any,
          runtime.TransformerCfg,
          runtime.TransformerFlags,
          dsls.TransformerDefinition
        ] { this: TransformerDefinition.type => }

    val PartialTransformerInto: PartialTransformerIntoModule
    trait PartialTransformerIntoModule
        extends Type.Ctor4UpperBounded[
          Any,
          Any,
          runtime.TransformerCfg,
          runtime.TransformerFlags,
          dsls.PartialTransformerInto
        ] { this: PartialTransformerInto.type => }

    val PartialTransformerDefinition: PartialTransformerDefinitionModule
    trait PartialTransformerDefinitionModule
        extends Type.Ctor4UpperBounded[
          Any,
          Any,
          runtime.TransformerCfg,
          runtime.TransformerFlags,
          dsls.PartialTransformerDefinition
        ] { this: PartialTransformerDefinition.type => }

    val RuntimeDataStore: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore]

    // You can import ChimneyType.Implicits.* in your shared code to avoid providing types manually, while avoiding conflicts
    // with implicit types seen in platform-specific scopes (which would happen if those implicits were always used).
    object Implicits {

      implicit def TransformerType[From: Type, To: Type]: Type[Transformer[From, To]] = Transformer[From, To]
      implicit def PartialTransformerType[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
        PartialTransformer[From, To]
      implicit def PatcherType[A: Type, Patch: Type]: Type[Patcher[A, Patch]] = Patcher[A, Patch]

      implicit def PartialResultType[A: Type]: Type[partial.Result[A]] = PartialResult[A]
      implicit def PartialResultValueType[A: Type]: Type[partial.Result.Value[A]] = PartialResult.Value[A]

      implicit val PartialResultErrorsType: Type[partial.Result.Errors] = PartialResult.Errors

      implicit val PathElementType: Type[partial.PathElement] = PathElement.tpe
      implicit val PathElementAccessorType: Type[partial.PathElement.Accessor] = PathElement.Accessor
      implicit val PathElementIndexType: Type[partial.PathElement.Index] = PathElement.Index
      implicit val PathElementMapKeyType: Type[partial.PathElement.MapKey] = PathElement.MapKey
      implicit val PathElementMapValueType: Type[partial.PathElement.MapValue] = PathElement.MapValue

      implicit val TransformerCfgEmptyType: Type[runtime.TransformerCfg.Empty] = TransformerCfg.Empty
      implicit def TransformerCfgFieldConstType[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
          : Type[runtime.TransformerCfg.FieldConst[Name, Cfg]] = TransformerCfg.FieldConst[Name, Cfg]
      implicit def TransformerCfgFieldComputedType[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
          : Type[runtime.TransformerCfg.FieldComputed[Name, Cfg]] = TransformerCfg.FieldComputed[Name, Cfg]
      implicit def TransformerCfgFieldConstPartialType[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
          : Type[runtime.TransformerCfg.FieldConstPartial[Name, Cfg]] = TransformerCfg.FieldConstPartial[Name, Cfg]
      implicit def TransformerCfgFieldComputedPartialType[Name <: String: Type, Cfg <: runtime.TransformerCfg: Type]
          : Type[runtime.TransformerCfg.FieldComputedPartial[Name, Cfg]] =
        TransformerCfg.FieldComputedPartial[Name, Cfg]
      implicit def TransformerCfgFieldRelabelledType[
          FromName <: String: Type,
          ToName <: String: Type,
          Cfg <: runtime.TransformerCfg: Type
      ]: Type[runtime.TransformerCfg.FieldRelabelled[FromName, ToName, Cfg]] =
        TransformerCfg.FieldRelabelled[FromName, ToName, Cfg]
      implicit def TransformerCfgCoproductInstanceType[
          InstType: Type,
          TargetType: Type,
          Cfg <: runtime.TransformerCfg: Type
      ]: Type[runtime.TransformerCfg.CoproductInstance[InstType, TargetType, Cfg]] =
        TransformerCfg.CoproductInstance[InstType, TargetType, Cfg]
      implicit def TransformerCfgCoproductInstancePartialType[
          InstType: Type,
          TargetType: Type,
          Cfg <: runtime.TransformerCfg: Type
      ]: Type[runtime.TransformerCfg.CoproductInstancePartial[InstType, TargetType, Cfg]] =
        TransformerCfg.CoproductInstancePartial[InstType, TargetType, Cfg]

      implicit val TransformerFlagsDefaultType: Type[runtime.TransformerFlags.Default] = TransformerFlags.Default
      implicit def TransformerFlagsEnableType[
          Flag <: runtime.TransformerFlags.Flag: Type,
          Flags <: runtime.TransformerFlags: Type
      ]: Type[runtime.TransformerFlags.Enable[Flag, Flags]] = TransformerFlags.Enable[Flag, Flags]
      implicit def TransformerFlagsDisableType[
          Flag <: runtime.TransformerFlags.Flag: Type,
          Flags <: runtime.TransformerFlags: Type
      ]: Type[runtime.TransformerFlags.Disable[Flag, Flags]] = TransformerFlags.Disable[Flag, Flags]

      implicit val TransformerFlagDefaultValuesType: Type[runtime.TransformerFlags.DefaultValues] =
        TransformerFlags.Flags.DefaultValues
      implicit val TransformerFlagBeanGettersType: Type[runtime.TransformerFlags.BeanGetters] =
        TransformerFlags.Flags.BeanGetters
      implicit val TransformerFlagBeanSettersType: Type[runtime.TransformerFlags.BeanSetters] =
        TransformerFlags.Flags.BeanSetters
      implicit val TransformerFlagMethodAccessorsType: Type[runtime.TransformerFlags.MethodAccessors] =
        TransformerFlags.Flags.MethodAccessors
      implicit val TransformerFlagOptionDefaultsToNoneType: Type[runtime.TransformerFlags.OptionDefaultsToNone] =
        TransformerFlags.Flags.OptionDefaultsToNone
      implicit def TransformerFlagImplicitConflictResolutionType[R <: dsls.ImplicitTransformerPreference: Type]
          : Type[runtime.TransformerFlags.ImplicitConflictResolution[R]] =
        TransformerFlags.Flags.ImplicitConflictResolution[R]
      implicit val TransformerFlagImplicitConflictResolutionType: Type[runtime.TransformerFlags.MacrosLogging] =
        TransformerFlags.Flags.MacrosLogging

      implicit def TransformerIntoType[
          From: Type,
          To: Type,
          Cfg <: runtime.TransformerCfg: Type,
          Flags <: runtime.TransformerFlags: Type
      ]: Type[dsls.TransformerInto[From, To, Cfg, Flags]] = TransformerInto[From, To, Cfg, Flags]
      implicit def TransformerDefinitionType[
          From: Type,
          To: Type,
          Cfg <: runtime.TransformerCfg: Type,
          Flags <: runtime.TransformerFlags: Type
      ]: Type[dsls.TransformerDefinition[From, To, Cfg, Flags]] = TransformerDefinition[From, To, Cfg, Flags]
      implicit def PartialTransformerIntoType[
          From: Type,
          To: Type,
          Cfg <: runtime.TransformerCfg: Type,
          Flags <: runtime.TransformerFlags: Type
      ]: Type[dsls.PartialTransformerInto[From, To, Cfg, Flags]] = PartialTransformerInto[From, To, Cfg, Flags]
      implicit def PartialTransformerDefinitionType[
          From: Type,
          To: Type,
          Cfg <: runtime.TransformerCfg: Type,
          Flags <: runtime.TransformerFlags: Type
      ]: Type[dsls.PartialTransformerDefinition[From, To, Cfg, Flags]] =
        PartialTransformerDefinition[From, To, Cfg, Flags]

      implicit val RuntimeDataStoreType: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore] = RuntimeDataStore
    }
  }
}
