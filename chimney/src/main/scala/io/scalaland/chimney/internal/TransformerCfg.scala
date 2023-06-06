package io.scalaland.chimney.internal

// TODO: move to internal.compiletime.derivation once macros are migrated
sealed abstract class TransformerCfg
object TransformerCfg {
  final class Empty extends TransformerCfg
  final class FieldConst[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldConstPartial[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldComputed[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldComputedPartial[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldRelabelled[FromName <: String, ToName <: String, C <: TransformerCfg] extends TransformerCfg
  final class CoproductInstance[InstType, TargetType, C <: TransformerCfg] extends TransformerCfg
  final class CoproductInstancePartial[InstType, TargetType, C <: TransformerCfg] extends TransformerCfg
}
