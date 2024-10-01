package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.dsl.{ImplicitTransformerPreference, ActionOnUnused, TransformedNamesComparison}

sealed abstract class TransformerFlags
object TransformerFlags {
  final class Default extends TransformerFlags
  final class Enable[F <: Flag, Flags <: TransformerFlags] extends TransformerFlags
  final class Disable[F <: Flag, Flags <: TransformerFlags] extends TransformerFlags

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
  final class ImplicitConflictResolution[R <: ImplicitTransformerPreference] extends Flag
  final class UnusedFieldPolicy[R <: ActionOnUnused] extends Flag
  final class FieldNameComparison[C <: TransformedNamesComparison] extends Flag
  final class SubtypeNameComparison[C <: TransformedNamesComparison] extends Flag
  final class MacrosLogging extends Flag
}
