package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.integrations
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
      val Const: Type[partial.PathElement.Const]
      val Computed: Type[partial.PathElement.Computed]
    }

    val PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    val PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    val SourceOrElseFallback: Type[io.scalaland.chimney.dsl.SourceOrElseFallback.type]
    val FallbackOrElseSource: Type[io.scalaland.chimney.dsl.FallbackOrElseSource.type]

    val SourceAppendFallback: Type[io.scalaland.chimney.dsl.SourceAppendFallback.type]
    val FallbackAppendSource: Type[io.scalaland.chimney.dsl.FallbackAppendSource.type]

    val FailOnIgnoredSourceVal: Type[io.scalaland.chimney.dsl.FailOnIgnoredSourceVal.type]

    val FailOnUnmatchedTargetSubtype: Type[io.scalaland.chimney.dsl.FailOnUnmatchedTargetSubtype.type]

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

      val Unused: UnusedModule
      trait UnusedModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.Unused
          ] { this: Unused.type => }

      val Unmatched: UnmatchedModule
      trait UnmatchedModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.Unmatched
          ] { this: Unmatched.type => }

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
          extends Type.Ctor3UpperBounded[
            runtime.Path,
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.Computed
          ] { this: Computed.type => }

      val ComputedPartial: ComputedPartialModule
      trait ComputedPartialModule
          extends Type.Ctor3UpperBounded[
            runtime.Path,
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.ComputedPartial
          ] { this: ComputedPartial.type => }

      val Fallback: FallbackModule
      trait FallbackModule
          extends Type.Ctor3UpperBounded[
            Any,
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.Fallback
          ] { this: Fallback.type => }

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

      val Renamed: RenamedModule
      trait RenamedModule
          extends Type.Ctor3UpperBounded[
            runtime.Path,
            runtime.Path,
            runtime.TransformerOverrides,
            runtime.TransformerOverrides.Renamed
          ] { this: Renamed.type => }
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

      val Source: SourceModule
      trait SourceModule
          extends Type.Ctor3UpperBounded[
            runtime.Path,
            runtime.TransformerFlags,
            runtime.TransformerFlags,
            runtime.TransformerFlags.Source
          ] { this: Source.type => }

      val Target: TargetModule
      trait TargetModule
          extends Type.Ctor3UpperBounded[
            runtime.Path,
            runtime.TransformerFlags,
            runtime.TransformerFlags,
            runtime.TransformerFlags.Target
          ] { this: Target.type => }

      val Flags: FlagsModule
      trait FlagsModule { this: Flags.type =>
        val InheritedAccessors: Type[runtime.TransformerFlags.InheritedAccessors]
        val MethodAccessors: Type[runtime.TransformerFlags.MethodAccessors]
        val DefaultValues: Type[runtime.TransformerFlags.DefaultValues]
        val DefaultValueOfType: DefaultValueOfTypeModule
        trait DefaultValueOfTypeModule
            extends Type.Ctor1[
              runtime.TransformerFlags.DefaultValueOfType
            ] { this: DefaultValueOfType.type => }
        val BeanGetters: Type[runtime.TransformerFlags.BeanGetters]
        val BeanSetters: Type[runtime.TransformerFlags.BeanSetters]
        val BeanSettersIgnoreUnmatched: Type[runtime.TransformerFlags.BeanSettersIgnoreUnmatched]
        val NonUnitBeanSetters: Type[runtime.TransformerFlags.NonUnitBeanSetters]
        val OptionDefaultsToNone: Type[runtime.TransformerFlags.OptionDefaultsToNone]
        val PartialUnwrapsOption: Type[runtime.TransformerFlags.PartialUnwrapsOption]
        val NonAnyValWrappers: Type[runtime.TransformerFlags.NonAnyValWrappers]
        val TypeConstraintEvidence: Type[runtime.TransformerFlags.TypeConstraintEvidence]
        val ImplicitConversions: Type[runtime.TransformerFlags.ImplicitConversions]
        val ImplicitConflictResolution: ImplicitConflictResolutionModule
        trait ImplicitConflictResolutionModule
            extends Type.Ctor1UpperBounded[
              dsls.ImplicitTransformerPreference,
              runtime.TransformerFlags.ImplicitConflictResolution
            ] { this: ImplicitConflictResolution.type => }
        val OptionFallbackMerge: OptionFallbackMergeModule
        trait OptionFallbackMergeModule
            extends Type.Ctor1UpperBounded[
              dsls.OptionFallbackMergeStrategy,
              runtime.TransformerFlags.OptionFallbackMerge
            ] { this: OptionFallbackMerge.type => }
        val EitherFallbackMerge: EitherFallbackMergeModule
        trait EitherFallbackMergeModule
            extends Type.Ctor1UpperBounded[
              dsls.OptionFallbackMergeStrategy,
              runtime.TransformerFlags.EitherFallbackMerge
            ] { this: EitherFallbackMerge.type => }
        val CollectionFallbackMerge: CollectionFallbackMergeModule
        trait CollectionFallbackMergeModule
            extends Type.Ctor1UpperBounded[
              dsls.CollectionFallbackMergeStrategy,
              runtime.TransformerFlags.CollectionFallbackMerge
            ] { this: CollectionFallbackMerge.type => }
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
        val UnusedFieldPolicyCheck: UnusedFieldPolicyCheckModule
        trait UnusedFieldPolicyCheckModule
            extends Type.Ctor1UpperBounded[
              dsls.UnusedFieldPolicy,
              runtime.TransformerFlags.UnusedFieldPolicyCheck
            ] { this: UnusedFieldPolicyCheck.type => }
        val UnmatchedSubtypePolicyCheck: UnmatchedSubtypePolicyCheckModule
        trait UnmatchedSubtypePolicyCheckModule
            extends Type.Ctor1UpperBounded[
              dsls.UnmatchedSubtypePolicy,
              runtime.TransformerFlags.UnmatchedSubtypePolicyCheck
            ] { this: UnmatchedSubtypePolicyCheck.type => }
        val MacrosLogging: Type[runtime.TransformerFlags.MacrosLogging]
      }
    }

    val PatcherOverrides: PatcherOverridesModule
    trait PatcherOverridesModule { this: PatcherOverrides.type =>
      val Empty: Type[runtime.PatcherOverrides.Empty]

      val Ignored: IgnoredModule
      trait IgnoredModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.PatcherOverrides,
            runtime.PatcherOverrides.Ignored
          ] { this: Ignored.type => }

      val Const: ConstModule
      trait ConstModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            runtime.PatcherOverrides,
            runtime.PatcherOverrides.Const
          ] { this: Const.type => }

      val Computed: ComputedModule
      trait ComputedModule
          extends Type.Ctor3UpperBounded[
            runtime.Path,
            runtime.Path,
            runtime.PatcherOverrides,
            runtime.PatcherOverrides.Computed
          ] { this: Computed.type => }
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

      val PatchedValue: PatchedValueModule
      trait PatchedValueModule
          extends Type.Ctor3UpperBounded[
            runtime.Path,
            runtime.PatcherFlags,
            runtime.PatcherFlags,
            runtime.PatcherFlags.PatchedValue
          ] { this: PatchedValue.type => }

      val Flags: FlagsModule
      trait FlagsModule { this: Flags.type =>
        val IgnoreNoneInPatch: Type[runtime.PatcherFlags.IgnoreNoneInPatch]
        val IgnoreLeftInPatch: Type[runtime.PatcherFlags.IgnoreLeftInPatch]
        val AppendCollectionInPatch: Type[runtime.PatcherFlags.AppendCollectionInPatch]
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
            runtime.Path,
            String,
            runtime.Path.Select
          ] { this: Select.type => }
      val Matching: MatchingModule
      trait MatchingModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            Any,
            runtime.Path.Matching
          ] { this: Matching.type => }
      val SourceMatching: SourceMatchingModule
      trait SourceMatchingModule
          extends Type.Ctor2UpperBounded[
            runtime.Path,
            Any,
            runtime.Path.SourceMatching
          ] { this: SourceMatching.type => }
      val EveryItem: EveryItemModule
      trait EveryItemModule
          extends Type.Ctor1UpperBounded[
            runtime.Path,
            runtime.Path.EveryItem
          ] { this: EveryItem.type => }
      val EveryMapKey: EveryMapKeyModule
      trait EveryMapKeyModule
          extends Type.Ctor1UpperBounded[
            runtime.Path,
            runtime.Path.EveryMapKey
          ] { this: EveryMapKey.type => }
      val EveryMapValue: EveryMapValueModule
      trait EveryMapValueModule
          extends Type.Ctor1UpperBounded[
            runtime.Path,
            runtime.Path.EveryMapValue
          ] { this: EveryMapValue.type => }
    }

    val PartialOuterTransformer: PartialOuterTransformerModule
    trait PartialOuterTransformerModule extends Type.Ctor4[integrations.PartialOuterTransformer] {
      this: PartialOuterTransformer.type =>
      def inferred[From: Type, To: Type]: ExistentialType
    }

    val TotalOuterTransformer: TotalOuterTransformerModule
    trait TotalOuterTransformerModule extends Type.Ctor4[integrations.TotalOuterTransformer] {
      this: TotalOuterTransformer.type =>
      def inferred[From: Type, To: Type]: ExistentialType
    }

    val DefaultValue: DefaultValueModule
    trait DefaultValueModule extends Type.Ctor1[integrations.DefaultValue] { this: DefaultValue.type => }

    val OptionalValue: OptionalValueModule
    trait OptionalValueModule extends Type.Ctor2[integrations.OptionalValue] { this: OptionalValue.type =>
      def inferred[Optional: Type]: ExistentialType
    }

    val PartiallyBuildIterable: PartiallyBuildIterableModule
    trait PartiallyBuildIterableModule extends Type.Ctor2[integrations.PartiallyBuildIterable] {
      this: PartiallyBuildIterable.type =>
      def inferred[Collection: Type]: ExistentialType
    }

    val PartiallyBuildMap: PartiallyBuildMapModule
    trait PartiallyBuildMapModule extends Type.Ctor3[integrations.PartiallyBuildMap] { this: PartiallyBuildMap.type => }

    val TotallyBuildIterable: TotallyBuildIterableModule
    trait TotallyBuildIterableModule extends Type.Ctor2[integrations.TotallyBuildIterable] {
      this: TotallyBuildIterable.type =>
      def inferred[Collection: Type]: ExistentialType
    }

    val TotallyBuildMap: TotallyBuildMapModule
    trait TotallyBuildMapModule extends Type.Ctor3[integrations.TotallyBuildMap] { this: TotallyBuildMap.type => }

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
      implicit val PathElementConst: Type[partial.PathElement.Const] = PathElement.Const
      implicit val PathElementComputed: Type[partial.PathElement.Computed] = PathElement.Computed

      implicit val RuntimeDataStoreType: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore] = RuntimeDataStore

      implicit def PartialOuterTransformerType[From: Type, To: Type, InnerFrom: Type, InnerTo: Type]
          : Type[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]] =
        PartialOuterTransformer[From, To, InnerFrom, InnerTo]
      implicit def TotalOuterTransformerType[From: Type, To: Type, InnerFrom: Type, InnerTo: Type]
          : Type[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]] =
        TotalOuterTransformer[From, To, InnerFrom, InnerTo]
      implicit def DefaultValueType[Value: Type]: Type[integrations.DefaultValue[Value]] = DefaultValue[Value]
      implicit def OptionalValueType[Optional: Type, Value: Type]: Type[integrations.OptionalValue[Optional, Value]] =
        OptionalValue[Optional, Value]
      implicit def PartiallyBuildIterableType[Optional: Type, Value: Type]
          : Type[integrations.PartiallyBuildIterable[Optional, Value]] = PartiallyBuildIterable[Optional, Value]
      implicit def TotallyBuildIterableType[Optional: Type, Value: Type]
          : Type[integrations.TotallyBuildIterable[Optional, Value]] = TotallyBuildIterable[Optional, Value]
    }
  }
}
