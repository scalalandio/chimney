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

    val TransformerOverrides: TransformerOverridesModule
    trait TransformerOverridesModule { this: TransformerOverrides.type =>
      val Empty: Type[runtime.TransformerOverrides.Empty]

      val Const: ConstModule
      trait ConstModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.Const
          ] { this: Const.type => }

      val ConstPartial: ConstPartialModule
      trait ConstPartialModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.ConstPartial
          ] { this: ConstPartial.type => }

      val Computed: ComputedModule
      trait ComputedModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.Computed
          ] { this: Computed.type => }

      val ComputedPartial: ComputedPartialModule
      trait ComputedPartialModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.ComputedPartial
          ] { this: ComputedPartial.type => }

      val CaseComputed: CaseComputedModule
      trait CaseComputedModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.CaseComputed
          ] { this: CaseComputed.type => }

      val CaseComputedPartial: CaseComputedPartialModule
      trait CaseComputedPartialModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.CaseComputedPartial
          ] { this: CaseComputedPartial.type => }

      val Constructor: ConstructorModule
      trait ConstructorModule
          extends Type.Ctor3UpperBounded[
            runtime.ArgumentLists,
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.Constructor
          ] { this: Constructor.type => }

      val ConstructorPartial: ConstructorPartialModule
      trait ConstructorPartialModule
          extends Type.Ctor3UpperBounded[
            runtime.ArgumentLists,
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.ConstructorPartial
          ] { this: ConstructorPartial.type => }

      val RenamedFrom: RenamedFromModule
      trait RenamedFromModule
          extends Type.Ctor3UpperBounded[
            runtime.Path,
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.RenamedFrom
          ] { this: RenamedFrom.type => }
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
        val NonUnitBeanSetters: Type[runtime.TransformerFlags.NonUnitBeanSetters]
        val OptionDefaultsToNone: Type[runtime.TransformerFlags.OptionDefaultsToNone]
        val PartialUnwrapsOption: Type[runtime.TransformerFlags.PartialUnwrapsOption]
        val ImplicitConflictResolution: ImplicitConflictResolutionModule
        trait ImplicitConflictResolutionModule
            extends Type.Ctor1UpperBounded[
              dsls.ImplicitTransformerPreference,
              runtime.TransformerFlags.ImplicitConflictResolution
            ] { this: ImplicitConflictResolution.type => }
        val FieldNameComparison: FieldNameComparisonModule
        trait FieldNameComparisonModule
            extends Type.Ctor1UpperBounded[
              dsls.TransformedNamesComparison,
              runtime.TransformerFlags.FieldNameComparison
            ] { this: FieldNameComparison.type => }
        val SubtypeNameComparison: SubtypeNameComparisonModule
        trait SubtypeNameComparisonModule
            extends Type.Ctor1UpperBounded[
              dsls.TransformedNamesComparison,
              runtime.TransformerFlags.SubtypeNameComparison
            ] { this: SubtypeNameComparison.type => }
        val MacrosLogging: Type[runtime.TransformerFlags.MacrosLogging]
      }
    }

    val PatcherOverrides: PatcherOverridesModule
    trait PatcherOverridesModule { this: PatcherOverrides.type =>
      val Empty: Type[runtime.PatcherOverrides.Empty]
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
      val Match: MatchModule
      trait MatchModule
          extends Type.Ctor2UpperBounded[
            Any,
            runtime.Path,
            runtime.Path.Match
          ] { this: Match.type => }
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
