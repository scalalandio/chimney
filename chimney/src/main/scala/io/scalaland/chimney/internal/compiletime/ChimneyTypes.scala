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

    val RuntimeDataStore: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore]

    val ArgumentList: ArgumentListModule
    trait ArgumentListModule { this: ArgumentList.type =>
      val Empty: Type[runtime.ArgumentList.Empty]

      val Argument: ArgumentModule
      trait ArgumentModule
          extends Type.Ctor3UpperBounded[
            String,
            Any,
            runtime.ArgumentList,
            runtime.ArgumentList.Argument
          ] { this: Argument.type => }
    }

    val ArgumentLists: ArgumentListsModule
    trait ArgumentListsModule { this: ArgumentLists.type =>
      val Empty: Type[runtime.ArgumentLists.Empty]

      val List: ListModule
      trait ListModule
          extends Type.Ctor2UpperBounded[
            runtime.ArgumentList,
            runtime.ArgumentLists,
            runtime.ArgumentLists.List
          ] { this: List.type => }
    }

    val TransformerCfg: TransformerCfgModule
    trait TransformerCfgModule { this: TransformerCfg.type =>
      val Empty: Type[runtime.TransformerCfg.Empty]

      val FieldConst: FieldConstModule
      trait FieldConstModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerCfg,
            runtime.TransformerCfg.FieldConst
          ] { this: FieldConst.type => }

      val FieldConstPartial: FieldConstPartialModule
      trait FieldConstPartialModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerCfg,
            runtime.TransformerCfg.FieldConstPartial
          ] { this: FieldConstPartial.type => }

      val FieldComputed: FieldComputedModule
      trait FieldComputedModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerCfg,
            runtime.TransformerCfg.FieldComputed
          ] { this: FieldComputed.type => }

      val FieldComputedPartial: FieldComputedPartialModule
      trait FieldComputedPartialModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerCfg,
            runtime.TransformerCfg.FieldComputedPartial
          ] { this: FieldComputedPartial.type => }

      val FieldRelabelled: FieldRelabelledModule
      trait FieldRelabelledModule
          extends Type.Ctor3UpperBounded[
            runtime.Path,
            runtime.Path,
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

      val Constructor: ConstructorModule
      trait ConstructorModule
          extends Type.Ctor3UpperBounded[
            runtime.ArgumentLists,
            Any,
            runtime.TransformerCfg,
            runtime.TransformerCfg.Constructor
          ] { this: Constructor.type => }

      val ConstructorPartial: ConstructorPartialModule
      trait ConstructorPartialModule
          extends Type.Ctor3UpperBounded[
            runtime.ArgumentLists,
            Any,
            runtime.TransformerCfg,
            runtime.TransformerCfg.ConstructorPartial
          ] { this: ConstructorPartial.type => }
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
        val InheritedAccessors: Type[runtime.TransformerFlags.InheritedAccessors]
        val MethodAccessors: Type[runtime.TransformerFlags.MethodAccessors]
        val DefaultValues: Type[runtime.TransformerFlags.DefaultValues]
        val BeanGetters: Type[runtime.TransformerFlags.BeanGetters]
        val BeanSetters: Type[runtime.TransformerFlags.BeanSetters]
        val BeanSettersIgnoreUnmatched: Type[runtime.TransformerFlags.BeanSettersIgnoreUnmatched]
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
    trait PatcherCfgModule { this: PatcherCfg.type =>
      val Empty: Type[runtime.PatcherCfg.Empty]
    }

    val PatcherFlags: PatcherFlagsModule
    trait PatcherFlagsModule { this: PatcherFlags.type =>
      val Default: Type[runtime.PatcherFlags.Default]

      val Enable: EnableModule
      trait EnableModule
          extends Type.Ctor2UpperBounded[
            runtime.PatcherFlags.Flag,
            runtime.PatcherFlags,
            runtime.PatcherFlags.Enable
          ] { this: Enable.type => }

      val Disable: DisableModule
      trait DisableModule
          extends Type.Ctor2UpperBounded[
            runtime.PatcherFlags.Flag,
            runtime.PatcherFlags,
            runtime.PatcherFlags.Disable
          ] { this: Disable.type => }

      val Flags: FlagsModule
      trait FlagsModule { this: Flags.type =>
        val IgnoreNoneInPatch: Type[runtime.PatcherFlags.IgnoreNoneInPatch]
        val IgnoreRedundantPatcherFields: Type[runtime.PatcherFlags.IgnoreRedundantPatcherFields]
        val MacrosLogging: Type[runtime.PatcherFlags.MacrosLogging]
      }
    }

    val Path: PathModule
    trait PathModule { this: Path.type =>
      val Root: Type[runtime.Path.Root]
      val Select: SelectModule
      trait SelectModule
          extends Type.Ctor2UpperBounded[
            String,
            runtime.Path,
            runtime.Path.Select
          ] { this: Select.type => }
    }

    // You can `import ChimneyType.Implicits.*` in your shared code to avoid providing types manually, while avoiding conflicts
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

      implicit val RuntimeDataStoreType: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore] = RuntimeDataStore
    }
  }
}
