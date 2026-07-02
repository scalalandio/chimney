package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.integrations
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

/** Hearth-based port of the pre-Hearth `io.scalaland.chimney.internal.compiletime.ChimneyTypes` - merges the shared
  * trait and both platform implementations into a single cross-quoted source.
  *
  * Member names/paths are preserved 1:1 with the macro-commons version so that rule code can be ported mechanically.
  * Differences:
  *   - modules that used to be hand-implemented `Type.CtorN(Bounded)` are now `Type.CtorN` instances (Hearth generates
  *     `apply`/`unapply` for them); upper-bounded ones go through `ctorNUpperBoundedCompat` (see [[MacroCommonsCompat]]
  *     for the Hearth 0.4.0 bug it works around),
  *   - `unapply` results use Hearth existential spellings (`??`, `??<:[U]`) instead of macro-commons (`??`, `?<[U]`),
  *   - `inferred` members hide their wildcards behind type aliases (cross-quotes `Type.of[F[A, ?]]` does not compile on
  *     Scala 2 - another Hearth 0.4.0 bug),
  *   - Scala-2-only `ChimneyType.platformSpecific.fixJavaEnum(s)` (the `runtime.RefinedJavaEnum` workaround) is NOT
  *     ported here - it is consumed only by the Scala 2 DSL macro entrypoints and will be revisited with them.
  */
private[compiletime] trait ChimneyTypes { this: ChimneyDefinitions & hearth.MacroCommons =>

  protected object ChimneyType {

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] =
      Type.of[Transformer[From, To]]

    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      Type.of[PartialTransformer[From, To]]

    def Patcher[A: Type, Patch: Type]: Type[Patcher[A, Patch]] =
      Type.of[Patcher[A, Patch]]

    object PartialResult extends Type.Ctor1[partial.Result] {
      private lazy val ctor: Type.Ctor1[partial.Result] = Type.Ctor1.of[partial.Result]

      def apply[A: Type]: Type[partial.Result[A]] = ctor[A]
      def unapply[In](In: Type[In]): Option[??] = ctor.unapply(In)
      override def asUntyped: UntypedType = ctor.asUntyped

      def Value[A: Type]: Type[partial.Result.Value[A]] = Type.of[partial.Result.Value[A]]
      lazy val Errors: Type[partial.Result.Errors] = Type.of[partial.Result.Errors]
    }

    object PathElement {
      lazy val tpe: Type[partial.PathElement] = Type.of[partial.PathElement]
      lazy val Accessor: Type[partial.PathElement.Accessor] = Type.of[partial.PathElement.Accessor]
      lazy val Index: Type[partial.PathElement.Index] = Type.of[partial.PathElement.Index]
      lazy val MapKey: Type[partial.PathElement.MapKey] = Type.of[partial.PathElement.MapKey]
      lazy val MapValue: Type[partial.PathElement.MapValue] = Type.of[partial.PathElement.MapValue]
      lazy val Const: Type[partial.PathElement.Const] = Type.of[partial.PathElement.Const]
      lazy val Computed: Type[partial.PathElement.Computed] = Type.of[partial.PathElement.Computed]
    }

    lazy val PreferTotalTransformer: Type[dsls.PreferTotalTransformer.type] =
      Type.of[dsls.PreferTotalTransformer.type]
    lazy val PreferPartialTransformer: Type[dsls.PreferPartialTransformer.type] =
      Type.of[dsls.PreferPartialTransformer.type]

    lazy val SourceOrElseFallback: Type[dsls.SourceOrElseFallback.type] =
      Type.of[dsls.SourceOrElseFallback.type]
    lazy val FallbackOrElseSource: Type[dsls.FallbackOrElseSource.type] =
      Type.of[dsls.FallbackOrElseSource.type]

    lazy val SourceAppendFallback: Type[dsls.SourceAppendFallback.type] =
      Type.of[dsls.SourceAppendFallback.type]
    lazy val FallbackAppendSource: Type[dsls.FallbackAppendSource.type] =
      Type.of[dsls.FallbackAppendSource.type]

    lazy val FailOnIgnoredSourceVal: Type[dsls.FailOnIgnoredSourceVal.type] =
      Type.of[dsls.FailOnIgnoredSourceVal.type]

    lazy val FailOnUnmatchedTargetSubtype: Type[dsls.FailOnUnmatchedTargetSubtype.type] =
      Type.of[dsls.FailOnUnmatchedTargetSubtype.type]

    lazy val RuntimeDataStore: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore] =
      Type.of[dsls.TransformerDefinitionCommons.RuntimeDataStore]

    object ArgumentList {
      lazy val Empty: Type[runtime.ArgumentList.Empty] = Type.of[runtime.ArgumentList.Empty]

      lazy val Argument: Type.Ctor3.UpperBounded[String, Any, runtime.ArgumentList, runtime.ArgumentList.Argument] =
        ctor3UpperBoundedCompat[String, Any, runtime.ArgumentList, runtime.ArgumentList.Argument](
          Type.of[runtime.ArgumentList.Argument[String, Any, runtime.ArgumentList]]
        )
    }

    object ArgumentLists {
      lazy val Empty: Type[runtime.ArgumentLists.Empty] = Type.of[runtime.ArgumentLists.Empty]

      lazy val List: Type.Ctor2.UpperBounded[runtime.ArgumentList, runtime.ArgumentLists, runtime.ArgumentLists.List] =
        ctor2UpperBoundedCompat[runtime.ArgumentList, runtime.ArgumentLists, runtime.ArgumentLists.List](
          Type.of[runtime.ArgumentLists.List[runtime.ArgumentList, runtime.ArgumentLists]]
        )
    }

    object TransformerOverrides {
      lazy val Empty: Type[runtime.TransformerOverrides.Empty] = Type.of[runtime.TransformerOverrides.Empty]

      lazy val Unused
          : Type.Ctor2.UpperBounded[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.Unused] =
        ctor2UpperBoundedCompat[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.Unused](
          Type.of[runtime.TransformerOverrides.Unused[runtime.Path, runtime.TransformerOverrides]]
        )

      lazy val Unmatched: Type.Ctor2.UpperBounded[
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.Unmatched
      ] =
        ctor2UpperBoundedCompat[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.Unmatched](
          Type.of[runtime.TransformerOverrides.Unmatched[runtime.Path, runtime.TransformerOverrides]]
        )

      lazy val Const
          : Type.Ctor2.UpperBounded[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.Const] =
        ctor2UpperBoundedCompat[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.Const](
          Type.of[runtime.TransformerOverrides.Const[runtime.Path, runtime.TransformerOverrides]]
        )

      lazy val ConstPartial: Type.Ctor2.UpperBounded[
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.ConstPartial
      ] =
        ctor2UpperBoundedCompat[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.ConstPartial](
          Type.of[runtime.TransformerOverrides.ConstPartial[runtime.Path, runtime.TransformerOverrides]]
        )

      lazy val Computed: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.Computed
      ] =
        ctor3UpperBoundedCompat[
          runtime.Path,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.Computed
        ](
          Type.of[runtime.TransformerOverrides.Computed[runtime.Path, runtime.Path, runtime.TransformerOverrides]]
        )

      lazy val ComputedPartial: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.ComputedPartial
      ] =
        ctor3UpperBoundedCompat[
          runtime.Path,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.ComputedPartial
        ](
          Type.of[
            runtime.TransformerOverrides.ComputedPartial[runtime.Path, runtime.Path, runtime.TransformerOverrides]
          ]
        )

      lazy val Fallback: Type.Ctor3.UpperBounded[
        Any,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.Fallback
      ] =
        ctor3UpperBoundedCompat[
          Any,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.Fallback
        ](
          Type.of[runtime.TransformerOverrides.Fallback[Any, runtime.Path, runtime.TransformerOverrides]]
        )

      lazy val Constructor: Type.Ctor3.UpperBounded[
        runtime.ArgumentLists,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.Constructor
      ] =
        ctor3UpperBoundedCompat[
          runtime.ArgumentLists,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.Constructor
        ](
          Type.of[
            runtime.TransformerOverrides.Constructor[runtime.ArgumentLists, runtime.Path, runtime.TransformerOverrides]
          ]
        )

      lazy val ConstructorPartial: Type.Ctor3.UpperBounded[
        runtime.ArgumentLists,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.ConstructorPartial
      ] =
        ctor3UpperBoundedCompat[
          runtime.ArgumentLists,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.ConstructorPartial
        ](
          Type.of[
            runtime.TransformerOverrides.ConstructorPartial[
              runtime.ArgumentLists,
              runtime.Path,
              runtime.TransformerOverrides
            ]
          ]
        )

      lazy val Renamed: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.Renamed
      ] =
        ctor3UpperBoundedCompat[
          runtime.Path,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.Renamed
        ](
          Type.of[runtime.TransformerOverrides.Renamed[runtime.Path, runtime.Path, runtime.TransformerOverrides]]
        )
    }

    object TransformerFlags {
      lazy val Default: Type[runtime.TransformerFlags.Default] = Type.of[runtime.TransformerFlags.Default]

      lazy val Enable: Type.Ctor2.UpperBounded[
        runtime.TransformerFlags.Flag,
        runtime.TransformerFlags,
        runtime.TransformerFlags.Enable
      ] =
        ctor2UpperBoundedCompat[
          runtime.TransformerFlags.Flag,
          runtime.TransformerFlags,
          runtime.TransformerFlags.Enable
        ](
          Type.of[runtime.TransformerFlags.Enable[runtime.TransformerFlags.Flag, runtime.TransformerFlags]]
        )

      lazy val Disable: Type.Ctor2.UpperBounded[
        runtime.TransformerFlags.Flag,
        runtime.TransformerFlags,
        runtime.TransformerFlags.Disable
      ] =
        ctor2UpperBoundedCompat[
          runtime.TransformerFlags.Flag,
          runtime.TransformerFlags,
          runtime.TransformerFlags.Disable
        ](
          Type.of[runtime.TransformerFlags.Disable[runtime.TransformerFlags.Flag, runtime.TransformerFlags]]
        )

      lazy val Source: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.TransformerFlags,
        runtime.TransformerFlags,
        runtime.TransformerFlags.Source
      ] =
        ctor3UpperBoundedCompat[
          runtime.Path,
          runtime.TransformerFlags,
          runtime.TransformerFlags,
          runtime.TransformerFlags.Source
        ](
          Type.of[runtime.TransformerFlags.Source[runtime.Path, runtime.TransformerFlags, runtime.TransformerFlags]]
        )

      lazy val Target: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.TransformerFlags,
        runtime.TransformerFlags,
        runtime.TransformerFlags.Target
      ] =
        ctor3UpperBoundedCompat[
          runtime.Path,
          runtime.TransformerFlags,
          runtime.TransformerFlags,
          runtime.TransformerFlags.Target
        ](
          Type.of[runtime.TransformerFlags.Target[runtime.Path, runtime.TransformerFlags, runtime.TransformerFlags]]
        )

      object Flags {
        lazy val InheritedAccessors: Type[runtime.TransformerFlags.InheritedAccessors] =
          Type.of[runtime.TransformerFlags.InheritedAccessors]
        lazy val MethodAccessors: Type[runtime.TransformerFlags.MethodAccessors] =
          Type.of[runtime.TransformerFlags.MethodAccessors]
        lazy val DefaultValues: Type[runtime.TransformerFlags.DefaultValues] =
          Type.of[runtime.TransformerFlags.DefaultValues]
        lazy val DefaultValueOfType: Type.Ctor1[runtime.TransformerFlags.DefaultValueOfType] =
          Type.Ctor1.of[runtime.TransformerFlags.DefaultValueOfType]
        lazy val BeanGetters: Type[runtime.TransformerFlags.BeanGetters] =
          Type.of[runtime.TransformerFlags.BeanGetters]
        lazy val BeanSetters: Type[runtime.TransformerFlags.BeanSetters] =
          Type.of[runtime.TransformerFlags.BeanSetters]
        lazy val BeanSettersIgnoreUnmatched: Type[runtime.TransformerFlags.BeanSettersIgnoreUnmatched] =
          Type.of[runtime.TransformerFlags.BeanSettersIgnoreUnmatched]
        lazy val NonUnitBeanSetters: Type[runtime.TransformerFlags.NonUnitBeanSetters] =
          Type.of[runtime.TransformerFlags.NonUnitBeanSetters]
        lazy val OptionDefaultsToNone: Type[runtime.TransformerFlags.OptionDefaultsToNone] =
          Type.of[runtime.TransformerFlags.OptionDefaultsToNone]
        lazy val PartialUnwrapsOption: Type[runtime.TransformerFlags.PartialUnwrapsOption] =
          Type.of[runtime.TransformerFlags.PartialUnwrapsOption]
        lazy val NonAnyValWrappers: Type[runtime.TransformerFlags.NonAnyValWrappers] =
          Type.of[runtime.TransformerFlags.NonAnyValWrappers]
        lazy val TypeConstraintEvidence: Type[runtime.TransformerFlags.TypeConstraintEvidence] =
          Type.of[runtime.TransformerFlags.TypeConstraintEvidence]
        lazy val ImplicitConversions: Type[runtime.TransformerFlags.ImplicitConversions] =
          Type.of[runtime.TransformerFlags.ImplicitConversions]
        lazy val ImplicitConflictResolution: Type.Ctor1.UpperBounded[
          dsls.ImplicitTransformerPreference,
          runtime.TransformerFlags.ImplicitConflictResolution
        ] =
          ctor1UpperBoundedCompat[
            dsls.ImplicitTransformerPreference,
            runtime.TransformerFlags.ImplicitConflictResolution
          ](
            Type.of[runtime.TransformerFlags.ImplicitConflictResolution[dsls.ImplicitTransformerPreference]]
          )
        lazy val OptionFallbackMerge: Type.Ctor1.UpperBounded[
          dsls.OptionFallbackMergeStrategy,
          runtime.TransformerFlags.OptionFallbackMerge
        ] =
          ctor1UpperBoundedCompat[dsls.OptionFallbackMergeStrategy, runtime.TransformerFlags.OptionFallbackMerge](
            Type.of[runtime.TransformerFlags.OptionFallbackMerge[dsls.OptionFallbackMergeStrategy]]
          )
        lazy val EitherFallbackMerge: Type.Ctor1.UpperBounded[
          dsls.OptionFallbackMergeStrategy,
          runtime.TransformerFlags.EitherFallbackMerge
        ] =
          ctor1UpperBoundedCompat[dsls.OptionFallbackMergeStrategy, runtime.TransformerFlags.EitherFallbackMerge](
            Type.of[runtime.TransformerFlags.EitherFallbackMerge[dsls.OptionFallbackMergeStrategy]]
          )
        lazy val CollectionFallbackMerge: Type.Ctor1.UpperBounded[
          dsls.CollectionFallbackMergeStrategy,
          runtime.TransformerFlags.CollectionFallbackMerge
        ] =
          ctor1UpperBoundedCompat[
            dsls.CollectionFallbackMergeStrategy,
            runtime.TransformerFlags.CollectionFallbackMerge
          ](
            Type.of[runtime.TransformerFlags.CollectionFallbackMerge[dsls.CollectionFallbackMergeStrategy]]
          )
        lazy val FieldNameComparison: Type.Ctor1.UpperBounded[
          dsls.TransformedNamesComparison,
          runtime.TransformerFlags.FieldNameComparison
        ] =
          ctor1UpperBoundedCompat[dsls.TransformedNamesComparison, runtime.TransformerFlags.FieldNameComparison](
            Type.of[runtime.TransformerFlags.FieldNameComparison[dsls.TransformedNamesComparison]]
          )
        lazy val SubtypeNameComparison: Type.Ctor1.UpperBounded[
          dsls.TransformedNamesComparison,
          runtime.TransformerFlags.SubtypeNameComparison
        ] =
          ctor1UpperBoundedCompat[dsls.TransformedNamesComparison, runtime.TransformerFlags.SubtypeNameComparison](
            Type.of[runtime.TransformerFlags.SubtypeNameComparison[dsls.TransformedNamesComparison]]
          )
        lazy val UnusedFieldPolicyCheck: Type.Ctor1.UpperBounded[
          dsls.UnusedFieldPolicy,
          runtime.TransformerFlags.UnusedFieldPolicyCheck
        ] =
          ctor1UpperBoundedCompat[dsls.UnusedFieldPolicy, runtime.TransformerFlags.UnusedFieldPolicyCheck](
            Type.of[runtime.TransformerFlags.UnusedFieldPolicyCheck[dsls.UnusedFieldPolicy]]
          )
        lazy val UnmatchedSubtypePolicyCheck: Type.Ctor1.UpperBounded[
          dsls.UnmatchedSubtypePolicy,
          runtime.TransformerFlags.UnmatchedSubtypePolicyCheck
        ] =
          ctor1UpperBoundedCompat[dsls.UnmatchedSubtypePolicy, runtime.TransformerFlags.UnmatchedSubtypePolicyCheck](
            Type.of[runtime.TransformerFlags.UnmatchedSubtypePolicyCheck[dsls.UnmatchedSubtypePolicy]]
          )
        lazy val MacrosLogging: Type[runtime.TransformerFlags.MacrosLogging] =
          Type.of[runtime.TransformerFlags.MacrosLogging]
      }
    }

    object PatcherOverrides {
      lazy val Empty: Type[runtime.PatcherOverrides.Empty] = Type.of[runtime.PatcherOverrides.Empty]

      lazy val Ignored
          : Type.Ctor2.UpperBounded[runtime.Path, runtime.PatcherOverrides, runtime.PatcherOverrides.Ignored] =
        ctor2UpperBoundedCompat[runtime.Path, runtime.PatcherOverrides, runtime.PatcherOverrides.Ignored](
          Type.of[runtime.PatcherOverrides.Ignored[runtime.Path, runtime.PatcherOverrides]]
        )

      lazy val Const: Type.Ctor2.UpperBounded[runtime.Path, runtime.PatcherOverrides, runtime.PatcherOverrides.Const] =
        ctor2UpperBoundedCompat[runtime.Path, runtime.PatcherOverrides, runtime.PatcherOverrides.Const](
          Type.of[runtime.PatcherOverrides.Const[runtime.Path, runtime.PatcherOverrides]]
        )

      lazy val Computed: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.Path,
        runtime.PatcherOverrides,
        runtime.PatcherOverrides.Computed
      ] =
        ctor3UpperBoundedCompat[
          runtime.Path,
          runtime.Path,
          runtime.PatcherOverrides,
          runtime.PatcherOverrides.Computed
        ](
          Type.of[runtime.PatcherOverrides.Computed[runtime.Path, runtime.Path, runtime.PatcherOverrides]]
        )
    }

    object PatcherFlags {
      lazy val Default: Type[runtime.PatcherFlags.Default] = Type.of[runtime.PatcherFlags.Default]

      lazy val Enable
          : Type.Ctor2.UpperBounded[runtime.PatcherFlags.Flag, runtime.PatcherFlags, runtime.PatcherFlags.Enable] =
        ctor2UpperBoundedCompat[runtime.PatcherFlags.Flag, runtime.PatcherFlags, runtime.PatcherFlags.Enable](
          Type.of[runtime.PatcherFlags.Enable[runtime.PatcherFlags.Flag, runtime.PatcherFlags]]
        )

      lazy val Disable
          : Type.Ctor2.UpperBounded[runtime.PatcherFlags.Flag, runtime.PatcherFlags, runtime.PatcherFlags.Disable] =
        ctor2UpperBoundedCompat[runtime.PatcherFlags.Flag, runtime.PatcherFlags, runtime.PatcherFlags.Disable](
          Type.of[runtime.PatcherFlags.Disable[runtime.PatcherFlags.Flag, runtime.PatcherFlags]]
        )

      lazy val PatchedValue: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.PatcherFlags,
        runtime.PatcherFlags,
        runtime.PatcherFlags.PatchedValue
      ] =
        ctor3UpperBoundedCompat[
          runtime.Path,
          runtime.PatcherFlags,
          runtime.PatcherFlags,
          runtime.PatcherFlags.PatchedValue
        ](
          Type.of[runtime.PatcherFlags.PatchedValue[runtime.Path, runtime.PatcherFlags, runtime.PatcherFlags]]
        )

      object Flags {
        lazy val IgnoreNoneInPatch: Type[runtime.PatcherFlags.IgnoreNoneInPatch] =
          Type.of[runtime.PatcherFlags.IgnoreNoneInPatch]
        lazy val IgnoreLeftInPatch: Type[runtime.PatcherFlags.IgnoreLeftInPatch] =
          Type.of[runtime.PatcherFlags.IgnoreLeftInPatch]
        lazy val AppendCollectionInPatch: Type[runtime.PatcherFlags.AppendCollectionInPatch] =
          Type.of[runtime.PatcherFlags.AppendCollectionInPatch]
        lazy val IgnoreRedundantPatcherFields: Type[runtime.PatcherFlags.IgnoreRedundantPatcherFields] =
          Type.of[runtime.PatcherFlags.IgnoreRedundantPatcherFields]
        lazy val MacrosLogging: Type[runtime.PatcherFlags.MacrosLogging] =
          Type.of[runtime.PatcherFlags.MacrosLogging]
      }
    }

    object Path {
      lazy val Root: Type[runtime.Path.Root] = Type.of[runtime.Path.Root]

      lazy val Select: Type.Ctor2.UpperBounded[runtime.Path, String, runtime.Path.Select] =
        ctor2UpperBoundedCompat[runtime.Path, String, runtime.Path.Select](
          Type.of[runtime.Path.Select[runtime.Path, String]]
        )

      lazy val Matching: Type.Ctor2.UpperBounded[runtime.Path, Any, runtime.Path.Matching] =
        ctor2UpperBoundedCompat[runtime.Path, Any, runtime.Path.Matching](
          Type.of[runtime.Path.Matching[runtime.Path, Any]]
        )

      lazy val SourceMatching: Type.Ctor2.UpperBounded[runtime.Path, Any, runtime.Path.SourceMatching] =
        ctor2UpperBoundedCompat[runtime.Path, Any, runtime.Path.SourceMatching](
          Type.of[runtime.Path.SourceMatching[runtime.Path, Any]]
        )

      lazy val EveryItem: Type.Ctor1.UpperBounded[runtime.Path, runtime.Path.EveryItem] =
        ctor1UpperBoundedCompat[runtime.Path, runtime.Path.EveryItem](
          Type.of[runtime.Path.EveryItem[runtime.Path]]
        )

      lazy val EveryMapKey: Type.Ctor1.UpperBounded[runtime.Path, runtime.Path.EveryMapKey] =
        ctor1UpperBoundedCompat[runtime.Path, runtime.Path.EveryMapKey](
          Type.of[runtime.Path.EveryMapKey[runtime.Path]]
        )

      lazy val EveryMapValue: Type.Ctor1.UpperBounded[runtime.Path, runtime.Path.EveryMapValue] =
        ctor1UpperBoundedCompat[runtime.Path, runtime.Path.EveryMapValue](
          Type.of[runtime.Path.EveryMapValue[runtime.Path]]
        )
    }

    object PartialOuterTransformer extends Type.Ctor4[integrations.PartialOuterTransformer] {
      // See reapplyLeadingTypeArgsCompat for why the wildcards are not just `Type.of[F[From, To, ?, ?]]`.
      private lazy val inferredUntyped: UntypedType =
        Type.of[integrations.PartialOuterTransformer[Any, Any, ?, ?]].asUntyped

      private lazy val ctor: Type.Ctor4[integrations.PartialOuterTransformer] =
        Type.Ctor4.of[integrations.PartialOuterTransformer]

      def apply[From: Type, To: Type, InnerFrom: Type, InnerTo: Type]
          : Type[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]] =
        ctor[From, To, InnerFrom, InnerTo]
      def unapply[In](In: Type[In]): Option[(??, ??, ??, ??)] = ctor.unapply(In)
      override def asUntyped: UntypedType = ctor.asUntyped

      def inferred[From: Type, To: Type]: ExistentialType =
        reapplyLeadingTypeArgsCompat(inferredUntyped, List(Type[From].asUntyped, Type[To].asUntyped)).as_??
    }

    object TotalOuterTransformer extends Type.Ctor4[integrations.TotalOuterTransformer] {
      // See reapplyLeadingTypeArgsCompat for why the wildcards are not just `Type.of[F[From, To, ?, ?]]`.
      private lazy val inferredUntyped: UntypedType =
        Type.of[integrations.TotalOuterTransformer[Any, Any, ?, ?]].asUntyped

      private lazy val ctor: Type.Ctor4[integrations.TotalOuterTransformer] =
        Type.Ctor4.of[integrations.TotalOuterTransformer]

      def apply[From: Type, To: Type, InnerFrom: Type, InnerTo: Type]
          : Type[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]] =
        ctor[From, To, InnerFrom, InnerTo]
      def unapply[In](In: Type[In]): Option[(??, ??, ??, ??)] = ctor.unapply(In)
      override def asUntyped: UntypedType = ctor.asUntyped

      def inferred[From: Type, To: Type]: ExistentialType =
        reapplyLeadingTypeArgsCompat(inferredUntyped, List(Type[From].asUntyped, Type[To].asUntyped)).as_??
    }

    lazy val DefaultValue: Type.Ctor1[integrations.DefaultValue] =
      Type.Ctor1.of[integrations.DefaultValue]

    object OptionalValue extends Type.Ctor2[integrations.OptionalValue] {
      // See reapplyLeadingTypeArgsCompat for why the wildcard is not just `Type.of[F[Optional, ?]]`.
      private lazy val inferredUntyped: UntypedType =
        Type.of[integrations.OptionalValue[Any, ?]].asUntyped

      private lazy val ctor: Type.Ctor2[integrations.OptionalValue] = Type.Ctor2.of[integrations.OptionalValue]

      def apply[Optional: Type, Value: Type]: Type[integrations.OptionalValue[Optional, Value]] =
        ctor[Optional, Value]
      def unapply[In](In: Type[In]): Option[(??, ??)] = ctor.unapply(In)
      override def asUntyped: UntypedType = ctor.asUntyped

      def inferred[Optional: Type]: ExistentialType =
        reapplyLeadingTypeArgsCompat(inferredUntyped, List(Type[Optional].asUntyped)).as_??
    }

    object PartiallyBuildIterable extends Type.Ctor2[integrations.PartiallyBuildIterable] {
      // See reapplyLeadingTypeArgsCompat for why the wildcard is not just `Type.of[F[Collection, ?]]`.
      private lazy val inferredUntyped: UntypedType =
        Type.of[integrations.PartiallyBuildIterable[Any, ?]].asUntyped

      private lazy val ctor: Type.Ctor2[integrations.PartiallyBuildIterable] =
        Type.Ctor2.of[integrations.PartiallyBuildIterable]

      def apply[Collection: Type, Item: Type]: Type[integrations.PartiallyBuildIterable[Collection, Item]] =
        ctor[Collection, Item]
      def unapply[In](In: Type[In]): Option[(??, ??)] = ctor.unapply(In)
      override def asUntyped: UntypedType = ctor.asUntyped

      def inferred[Collection: Type]: ExistentialType =
        reapplyLeadingTypeArgsCompat(inferredUntyped, List(Type[Collection].asUntyped)).as_??
    }

    lazy val PartiallyBuildMap: Type.Ctor3[integrations.PartiallyBuildMap] =
      Type.Ctor3.of[integrations.PartiallyBuildMap]

    object TotallyBuildIterable extends Type.Ctor2[integrations.TotallyBuildIterable] {
      // See reapplyLeadingTypeArgsCompat for why the wildcard is not just `Type.of[F[Collection, ?]]`.
      private lazy val inferredUntyped: UntypedType =
        Type.of[integrations.TotallyBuildIterable[Any, ?]].asUntyped

      private lazy val ctor: Type.Ctor2[integrations.TotallyBuildIterable] =
        Type.Ctor2.of[integrations.TotallyBuildIterable]

      def apply[Collection: Type, Item: Type]: Type[integrations.TotallyBuildIterable[Collection, Item]] =
        ctor[Collection, Item]
      def unapply[In](In: Type[In]): Option[(??, ??)] = ctor.unapply(In)
      override def asUntyped: UntypedType = ctor.asUntyped

      def inferred[Collection: Type]: ExistentialType =
        reapplyLeadingTypeArgsCompat(inferredUntyped, List(Type[Collection].asUntyped)).as_??
    }

    lazy val TotallyBuildMap: Type.Ctor3[integrations.TotallyBuildMap] =
      Type.Ctor3.of[integrations.TotallyBuildMap]

    // You can `import ChimneyType.Implicits.*` in your shared code to avoid providing types manually, while avoiding conflicts
    // with implicit types seen in platform-specific scopes (which would happen if those implicits were always used).
    object Implicits {

      implicit def TransformerType[From: Type, To: Type]: Type[Transformer[From, To]] = Transformer[From, To]

      implicit def PartialTransformerType[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
        PartialTransformer[From, To]

      implicit def PatcherType[A: Type, Patch: Type]: Type[Patcher[A, Patch]] = Patcher[A, Patch]

      implicit def PartialResultType[A: Type]: Type[partial.Result[A]] = PartialResult[A]

      implicit def PartialResultValueType[A: Type]: Type[partial.Result.Value[A]] = PartialResult.Value[A]

      implicit lazy val PartialResultErrorsType: Type[partial.Result.Errors] = PartialResult.Errors

      implicit lazy val PathElementType: Type[partial.PathElement] = PathElement.tpe
      implicit lazy val PathElementAccessor: Type[partial.PathElement.Accessor] = PathElement.Accessor
      implicit lazy val PathElementIndex: Type[partial.PathElement.Index] = PathElement.Index
      implicit lazy val PathElementMapKey: Type[partial.PathElement.MapKey] = PathElement.MapKey
      implicit lazy val PathElementMapValue: Type[partial.PathElement.MapValue] = PathElement.MapValue
      implicit lazy val PathElementConst: Type[partial.PathElement.Const] = PathElement.Const
      implicit lazy val PathElementComputed: Type[partial.PathElement.Computed] = PathElement.Computed

      implicit lazy val RuntimeDataStoreType: Type[dsls.TransformerDefinitionCommons.RuntimeDataStore] =
        RuntimeDataStore

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
