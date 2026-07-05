package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.integrations
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

/** NB: upper-bounded `Type.CtorN` modules use Hearth's direct `Type.CtorN.UpperBounded.of` (hearth#307/#344), and
  * `inferred` members hide their wildcards behind type aliases (cross-quotes `Type.of[F[A, ?]]` does not compile on
  * Scala 2).
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
        Type.Ctor3.UpperBounded.of[String, Any, runtime.ArgumentList, runtime.ArgumentList.Argument]
    }

    object ArgumentLists {
      lazy val Empty: Type[runtime.ArgumentLists.Empty] = Type.of[runtime.ArgumentLists.Empty]

      lazy val List: Type.Ctor2.UpperBounded[runtime.ArgumentList, runtime.ArgumentLists, runtime.ArgumentLists.List] =
        Type.Ctor2.UpperBounded.of[runtime.ArgumentList, runtime.ArgumentLists, runtime.ArgumentLists.List]
    }

    object TransformerOverrides {
      lazy val Empty: Type[runtime.TransformerOverrides.Empty] = Type.of[runtime.TransformerOverrides.Empty]

      lazy val Unused
          : Type.Ctor2.UpperBounded[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.Unused] =
        Type.Ctor2.UpperBounded.of[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.Unused]

      lazy val Unmatched: Type.Ctor2.UpperBounded[
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.Unmatched
      ] =
        Type.Ctor2.UpperBounded.of[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.Unmatched]

      lazy val Const
          : Type.Ctor2.UpperBounded[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.Const] =
        Type.Ctor2.UpperBounded.of[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.Const]

      lazy val ConstPartial: Type.Ctor2.UpperBounded[
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.ConstPartial
      ] =
        Type.Ctor2.UpperBounded
          .of[runtime.Path, runtime.TransformerOverrides, runtime.TransformerOverrides.ConstPartial]

      lazy val Computed: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.Computed
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.Path,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.Computed
        ]

      lazy val ComputedPartial: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.ComputedPartial
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.Path,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.ComputedPartial
        ]

      lazy val Fallback: Type.Ctor3.UpperBounded[
        Any,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.Fallback
      ] =
        Type.Ctor3.UpperBounded.of[
          Any,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.Fallback
        ]

      lazy val Constructor: Type.Ctor3.UpperBounded[
        runtime.ArgumentLists,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.Constructor
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.ArgumentLists,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.Constructor
        ]

      lazy val ComputedPartialFailFast: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.ComputedPartialFailFast
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.Path,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.ComputedPartialFailFast
        ]

      lazy val ConstructorPartial: Type.Ctor3.UpperBounded[
        runtime.ArgumentLists,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.ConstructorPartial
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.ArgumentLists,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.ConstructorPartial
        ]

      lazy val ConstructorPartialFailFast: Type.Ctor3.UpperBounded[
        runtime.ArgumentLists,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.ConstructorPartialFailFast
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.ArgumentLists,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.ConstructorPartialFailFast
        ]

      lazy val Renamed: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.Path,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides.Renamed
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.Path,
          runtime.Path,
          runtime.TransformerOverrides,
          runtime.TransformerOverrides.Renamed
        ]

      // ForAll has 2 unbounded + 2 upper-bounded params, which does not fit the Type.CtorN(.UpperBounded) shapes -
      // a hand-rolled extractor on the untyped API (same exact-ctor matching as Type.CtorN.UpperBounded.of).
      object ForAll {
        private lazy val untypedCtor: UntypedType =
          UntypedType.typeConstructor(
            Type
              .of[
                runtime.TransformerOverrides.ForAll[
                  Any,
                  Any,
                  runtime.TransformerOverrides,
                  runtime.TransformerOverrides
                ]
              ]
              .asUntyped
          )

        def unapply[A](
            A: Type[A]
        ): Option[(??, ??, ??<:[runtime.TransformerOverrides], ??<:[runtime.TransformerOverrides])] = {
          val dealiased = UntypedType.dealias(A.asUntyped)
          if (UntypedType.sameTypeConstructorAs(untypedCtor, dealiased))
            UntypedType.typeArguments(dealiased) match {
              case a1 :: a2 :: a3 :: a4 :: Nil =>
                Some(
                  (
                    a1.asTyped[Any].as_??,
                    a2.asTyped[Any].as_??,
                    a3.asTyped[runtime.TransformerOverrides].as_??<:[runtime.TransformerOverrides],
                    a4.asTyped[runtime.TransformerOverrides].as_??<:[runtime.TransformerOverrides]
                  )
                )
              case _ => None
            }
          else None
        }
      }
    }

    object TransformerFlags {
      lazy val Default: Type[runtime.TransformerFlags.Default] = Type.of[runtime.TransformerFlags.Default]

      lazy val Enable: Type.Ctor2.UpperBounded[
        runtime.TransformerFlags.Flag,
        runtime.TransformerFlags,
        runtime.TransformerFlags.Enable
      ] =
        Type.Ctor2.UpperBounded.of[
          runtime.TransformerFlags.Flag,
          runtime.TransformerFlags,
          runtime.TransformerFlags.Enable
        ]

      lazy val Disable: Type.Ctor2.UpperBounded[
        runtime.TransformerFlags.Flag,
        runtime.TransformerFlags,
        runtime.TransformerFlags.Disable
      ] =
        Type.Ctor2.UpperBounded.of[
          runtime.TransformerFlags.Flag,
          runtime.TransformerFlags,
          runtime.TransformerFlags.Disable
        ]

      lazy val Source: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.TransformerFlags,
        runtime.TransformerFlags,
        runtime.TransformerFlags.Source
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.Path,
          runtime.TransformerFlags,
          runtime.TransformerFlags,
          runtime.TransformerFlags.Source
        ]

      lazy val Target: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.TransformerFlags,
        runtime.TransformerFlags,
        runtime.TransformerFlags.Target
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.Path,
          runtime.TransformerFlags,
          runtime.TransformerFlags,
          runtime.TransformerFlags.Target
        ]

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
          Type.Ctor1.UpperBounded.of[
            dsls.ImplicitTransformerPreference,
            runtime.TransformerFlags.ImplicitConflictResolution
          ]
        lazy val OptionFallbackMerge: Type.Ctor1.UpperBounded[
          dsls.OptionFallbackMergeStrategy,
          runtime.TransformerFlags.OptionFallbackMerge
        ] =
          Type.Ctor1.UpperBounded.of[dsls.OptionFallbackMergeStrategy, runtime.TransformerFlags.OptionFallbackMerge]
        lazy val EitherFallbackMerge: Type.Ctor1.UpperBounded[
          dsls.OptionFallbackMergeStrategy,
          runtime.TransformerFlags.EitherFallbackMerge
        ] =
          Type.Ctor1.UpperBounded.of[dsls.OptionFallbackMergeStrategy, runtime.TransformerFlags.EitherFallbackMerge]
        lazy val CollectionFallbackMerge: Type.Ctor1.UpperBounded[
          dsls.CollectionFallbackMergeStrategy,
          runtime.TransformerFlags.CollectionFallbackMerge
        ] =
          Type.Ctor1.UpperBounded.of[
            dsls.CollectionFallbackMergeStrategy,
            runtime.TransformerFlags.CollectionFallbackMerge
          ]
        lazy val FieldNameComparison: Type.Ctor1.UpperBounded[
          dsls.TransformedNamesComparison,
          runtime.TransformerFlags.FieldNameComparison
        ] =
          Type.Ctor1.UpperBounded.of[dsls.TransformedNamesComparison, runtime.TransformerFlags.FieldNameComparison]
        lazy val SubtypeNameComparison: Type.Ctor1.UpperBounded[
          dsls.TransformedNamesComparison,
          runtime.TransformerFlags.SubtypeNameComparison
        ] =
          Type.Ctor1.UpperBounded.of[dsls.TransformedNamesComparison, runtime.TransformerFlags.SubtypeNameComparison]
        lazy val UnusedFieldPolicyCheck: Type.Ctor1.UpperBounded[
          dsls.UnusedFieldPolicy,
          runtime.TransformerFlags.UnusedFieldPolicyCheck
        ] =
          Type.Ctor1.UpperBounded.of[dsls.UnusedFieldPolicy, runtime.TransformerFlags.UnusedFieldPolicyCheck]
        lazy val UnmatchedSubtypePolicyCheck: Type.Ctor1.UpperBounded[
          dsls.UnmatchedSubtypePolicy,
          runtime.TransformerFlags.UnmatchedSubtypePolicyCheck
        ] =
          Type.Ctor1.UpperBounded.of[dsls.UnmatchedSubtypePolicy, runtime.TransformerFlags.UnmatchedSubtypePolicyCheck]
        lazy val MacrosLogging: Type[runtime.TransformerFlags.MacrosLogging] =
          Type.of[runtime.TransformerFlags.MacrosLogging]
      }
    }

    object PatcherOverrides {
      lazy val Empty: Type[runtime.PatcherOverrides.Empty] = Type.of[runtime.PatcherOverrides.Empty]

      lazy val Ignored
          : Type.Ctor2.UpperBounded[runtime.Path, runtime.PatcherOverrides, runtime.PatcherOverrides.Ignored] =
        Type.Ctor2.UpperBounded.of[runtime.Path, runtime.PatcherOverrides, runtime.PatcherOverrides.Ignored]

      lazy val Const: Type.Ctor2.UpperBounded[runtime.Path, runtime.PatcherOverrides, runtime.PatcherOverrides.Const] =
        Type.Ctor2.UpperBounded.of[runtime.Path, runtime.PatcherOverrides, runtime.PatcherOverrides.Const]

      lazy val Computed: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.Path,
        runtime.PatcherOverrides,
        runtime.PatcherOverrides.Computed
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.Path,
          runtime.Path,
          runtime.PatcherOverrides,
          runtime.PatcherOverrides.Computed
        ]
    }

    object PatcherFlags {
      lazy val Default: Type[runtime.PatcherFlags.Default] = Type.of[runtime.PatcherFlags.Default]

      lazy val Enable
          : Type.Ctor2.UpperBounded[runtime.PatcherFlags.Flag, runtime.PatcherFlags, runtime.PatcherFlags.Enable] =
        Type.Ctor2.UpperBounded.of[runtime.PatcherFlags.Flag, runtime.PatcherFlags, runtime.PatcherFlags.Enable]

      lazy val Disable
          : Type.Ctor2.UpperBounded[runtime.PatcherFlags.Flag, runtime.PatcherFlags, runtime.PatcherFlags.Disable] =
        Type.Ctor2.UpperBounded.of[runtime.PatcherFlags.Flag, runtime.PatcherFlags, runtime.PatcherFlags.Disable]

      lazy val PatchedValue: Type.Ctor3.UpperBounded[
        runtime.Path,
        runtime.PatcherFlags,
        runtime.PatcherFlags,
        runtime.PatcherFlags.PatchedValue
      ] =
        Type.Ctor3.UpperBounded.of[
          runtime.Path,
          runtime.PatcherFlags,
          runtime.PatcherFlags,
          runtime.PatcherFlags.PatchedValue
        ]

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
        Type.Ctor2.UpperBounded.of[runtime.Path, String, runtime.Path.Select]

      lazy val Matching: Type.Ctor2.UpperBounded[runtime.Path, Any, runtime.Path.Matching] =
        Type.Ctor2.UpperBounded.of[runtime.Path, Any, runtime.Path.Matching]

      lazy val SourceMatching: Type.Ctor2.UpperBounded[runtime.Path, Any, runtime.Path.SourceMatching] =
        Type.Ctor2.UpperBounded.of[runtime.Path, Any, runtime.Path.SourceMatching]

      lazy val EveryItem: Type.Ctor1.UpperBounded[runtime.Path, runtime.Path.EveryItem] =
        Type.Ctor1.UpperBounded.of[runtime.Path, runtime.Path.EveryItem]

      lazy val EveryMapKey: Type.Ctor1.UpperBounded[runtime.Path, runtime.Path.EveryMapKey] =
        Type.Ctor1.UpperBounded.of[runtime.Path, runtime.Path.EveryMapKey]

      lazy val EveryMapValue: Type.Ctor1.UpperBounded[runtime.Path, runtime.Path.EveryMapValue] =
        Type.Ctor1.UpperBounded.of[runtime.Path, runtime.Path.EveryMapValue]
    }

    /** Scala 2's carrier of a Java-enum value selected in the DSL (see [[runtime.RefinedJavaEnum]]): built by
      * `DslDefinitions.javaEnumFixedSubtype`, decoded back by `Configurations.extractPath` - both through this ctor.
      */
    lazy val RefinedJavaEnum: Type.Ctor2.UpperBounded[Any, String, runtime.RefinedJavaEnum] =
      Type.Ctor2.UpperBounded.of[Any, String, runtime.RefinedJavaEnum]

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
