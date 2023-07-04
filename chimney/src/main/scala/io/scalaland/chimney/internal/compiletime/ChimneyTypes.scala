package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.*
import io.scalaland.chimney.dsl.TransformerDefinitionCommons.RuntimeDataStore
import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, TransformerDefinitionCommons}

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

    val RuntimeDataStore: Type[TransformerDefinitionCommons.RuntimeDataStore]

    val TransformerCfg: TransformerCfgModule
    trait TransformerCfgModule {
      val Empty: Type[internal.TransformerCfg.Empty]

      val FieldConst: FieldConstModule
      trait FieldConstModule
          extends Type.Ctor2UpperBounded[
            String,
            internal.TransformerCfg,
            internal.TransformerCfg.FieldConst
          ] { this: FieldConst.type => }

      val FieldConstPartial: FieldConstPartialModule
      trait FieldConstPartialModule
          extends Type.Ctor2UpperBounded[
            String,
            internal.TransformerCfg,
            internal.TransformerCfg.FieldConstPartial
          ] { this: FieldConstPartial.type => }

      val FieldComputed: FieldComputedModule
      trait FieldComputedModule
          extends Type.Ctor2UpperBounded[
            String,
            internal.TransformerCfg,
            internal.TransformerCfg.FieldComputed
          ] { this: FieldComputed.type => }

      val FieldComputedPartial: FieldComputedPartialModule
      trait FieldComputedPartialModule
          extends Type.Ctor2UpperBounded[
            String,
            internal.TransformerCfg,
            internal.TransformerCfg.FieldComputedPartial
          ] { this: FieldComputedPartial.type => }

      val FieldRelabelled: FieldRelabelledModule
      trait FieldRelabelledModule
          extends Type.Ctor3UpperBounded[
            String,
            String,
            internal.TransformerCfg,
            internal.TransformerCfg.FieldRelabelled
          ] { this: FieldRelabelled.type => }

      val CoproductInstance: CoproductInstanceModule
      trait CoproductInstanceModule
          extends Type.Ctor3UpperBounded[
            Any,
            Any,
            internal.TransformerCfg,
            internal.TransformerCfg.CoproductInstance
          ] { this: CoproductInstance.type => }

      val CoproductInstancePartial: CoproductInstancePartialModule
      trait CoproductInstancePartialModule
          extends Type.Ctor3UpperBounded[
            Any,
            Any,
            internal.TransformerCfg,
            internal.TransformerCfg.CoproductInstancePartial
          ] { this: CoproductInstancePartial.type => }
    }

    val TransformerFlags: TransformerFlagsModule
    trait TransformerFlagsModule { this: TransformerFlags.type =>
      val Default: Type[internal.TransformerFlags.Default]

      val Enable: EnableModule
      trait EnableModule
          extends Type.Ctor2UpperBounded[
            internal.TransformerFlags.Flag,
            internal.TransformerFlags,
            internal.TransformerFlags.Enable
          ] { this: Enable.type => }

      val Disable: DisableModule
      trait DisableModule
          extends Type.Ctor2UpperBounded[
            internal.TransformerFlags.Flag,
            internal.TransformerFlags,
            internal.TransformerFlags.Disable
          ] { this: Disable.type => }

      val Flags: FlagsModule
      trait FlagsModule { this: Flags.type =>
        val DefaultValues: Type[internal.TransformerFlags.DefaultValues]
        val BeanGetters: Type[internal.TransformerFlags.BeanGetters]
        val BeanSetters: Type[internal.TransformerFlags.BeanSetters]
        val MethodAccessors: Type[internal.TransformerFlags.MethodAccessors]
        val OptionDefaultsToNone: Type[internal.TransformerFlags.OptionDefaultsToNone]
        val ImplicitConflictResolution: ImplicitConflictResolutionModule
        trait ImplicitConflictResolutionModule
            extends Type.Ctor1UpperBounded[
              ImplicitTransformerPreference,
              internal.TransformerFlags.ImplicitConflictResolution
            ] { this: ImplicitConflictResolution.type => }
        val MacrosLogging: Type[internal.TransformerFlags.MacrosLogging]
      }
    }

    val PatcherCfg: PatcherCfgModule

    trait PatcherCfgModule {
      val Empty: Type[internal.PatcherCfg.Empty]

      val IgnoreRedundantPatcherFields: IgnoreRedundantPatcherFieldsModule

      trait IgnoreRedundantPatcherFieldsModule
          extends Type.Ctor1UpperBounded[
            internal.PatcherCfg,
            internal.PatcherCfg.IgnoreRedundantPatcherFields
          ] {
        this: IgnoreRedundantPatcherFields.type =>
      }

      val IgnoreNoneInPatch: IgnoreNoneInPatchModule

      trait IgnoreNoneInPatchModule
          extends Type.Ctor1UpperBounded[
            internal.PatcherCfg,
            internal.PatcherCfg.IgnoreNoneInPatch
          ] { this: IgnoreNoneInPatch.type => }

      val MacrosLogging: MacrosLoggingModule
      trait MacrosLoggingModule
          extends Type.Ctor1UpperBounded[
            internal.PatcherCfg,
            internal.PatcherCfg.MacrosLogging
          ] { this: MacrosLogging.type => }
    }

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
      implicit val PathElementAccessor: Type[partial.PathElement.Accessor] = PathElement.Accessor
      implicit val PathElementIndex: Type[partial.PathElement.Index] = PathElement.Index
      implicit val PathElementMapKey: Type[partial.PathElement.MapKey] = PathElement.MapKey
      implicit val PathElementMapValue: Type[partial.PathElement.MapValue] = PathElement.MapValue

      implicit val RuntimeDataStoreType: Type[RuntimeDataStore] = RuntimeDataStore
    }
  }
}
