package io.scalaland.chimney.internal

sealed abstract class TransformerFlags
object TransformerFlags {
  final class Default extends TransformerFlags
  final class Enable[F <: Flag, Flags <: TransformerFlags] extends TransformerFlags
  final class Disable[F <: Flag, Flags <: TransformerFlags] extends TransformerFlags

  sealed abstract class Flag
  final class MethodAccessors extends Flag
  final class DefaultValues extends Flag
  final class BeanSetters extends Flag
  final class BeanGetters extends Flag
  final class OptionDefaultsToNone extends Flag
  final class UnsafeOption extends Flag
}
