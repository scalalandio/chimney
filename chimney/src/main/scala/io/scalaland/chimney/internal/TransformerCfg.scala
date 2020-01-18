package io.scalaland.chimney.internal

sealed abstract class TransformerCfg

object TransformerCfg {
  final class Empty extends TransformerCfg
  final class DisableDefaultValues[C <: TransformerCfg] extends TransformerCfg
  final class EnableBeanGetters[C <: TransformerCfg] extends TransformerCfg
  final class EnableBeanSetters[C <: TransformerCfg] extends TransformerCfg
  final class EnableOptionDefaultsToNone[C <: TransformerCfg] extends TransformerCfg
  final class EnableUnsafeOption[C <: TransformerCfg] extends TransformerCfg
  final class FieldConst[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldComputed[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldRelabelled[FromName <: String, ToName <: String, C <: TransformerCfg] extends TransformerCfg
  final class CoproductInstance[InstType, TargetType, C <: TransformerCfg] extends TransformerCfg
}
