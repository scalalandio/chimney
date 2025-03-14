package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.dsl.{
  CollectionFallbackMergeStrategy,
  ImplicitTransformerPreference,
  OptionFallbackMergeStrategy,
  TransformedNamesComparison,
  UnmatchedSubtypePolicy,
  UnusedFieldPolicy
}

sealed abstract class TransformerFlags
object TransformerFlags {
  final class Default extends TransformerFlags
  final class Enable[F <: Flag, Flags <: TransformerFlags] extends TransformerFlags
  final class Disable[F <: Flag, Flags <: TransformerFlags] extends TransformerFlags
  final class Source[SourcePath <: Path, SourceFlags <: TransformerFlags, Flags <: TransformerFlags]
      extends TransformerFlags
  final class Target[TargetPath <: Path, TargetFlags <: TransformerFlags, Flags <: TransformerFlags]
      extends TransformerFlags

  sealed abstract class Flag
  final class InheritedAccessors extends Flag
  final class MethodAccessors extends Flag
  final class DefaultValues extends Flag
  final class DefaultValueOfType[T] extends Flag
  final class BeanSetters extends Flag
  final class BeanSettersIgnoreUnmatched extends Flag
  final class NonUnitBeanSetters extends Flag
  final class BeanGetters extends Flag
  final class OptionDefaultsToNone extends Flag
  final class PartialUnwrapsOption extends Flag
  final class NonAnyValWrappers extends Flag
  final class TypeConstraintEvidence extends Flag
  final class ImplicitConversions extends Flag
  final class ImplicitConflictResolution[R <: ImplicitTransformerPreference] extends Flag
  final class OptionFallbackMerge[S <: OptionFallbackMergeStrategy] extends Flag
  final class EitherFallbackMerge[S <: OptionFallbackMergeStrategy] extends Flag
  final class CollectionFallbackMerge[S <: CollectionFallbackMergeStrategy] extends Flag
  final class FieldNameComparison[C <: TransformedNamesComparison] extends Flag
  final class SubtypeNameComparison[C <: TransformedNamesComparison] extends Flag
  final class UnusedFieldPolicyCheck[P <: UnusedFieldPolicy] extends Flag
  final class UnmatchedSubtypePolicyCheck[P <: UnmatchedSubtypePolicy] extends Flag
  final class MacrosLogging extends Flag
}
