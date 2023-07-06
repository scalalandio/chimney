package io.scalaland.chimney.internal.runtime

sealed abstract class TransformerCfg
object TransformerCfg {
  final class Empty extends TransformerCfg
  final class FieldConst[Name <: String, Cfg <: TransformerCfg] extends TransformerCfg
  final class FieldConstPartial[Name <: String, Cfg <: TransformerCfg] extends TransformerCfg
  final class FieldComputed[Name <: String, Cfg <: TransformerCfg] extends TransformerCfg
  final class FieldComputedPartial[Name <: String, Cfg <: TransformerCfg] extends TransformerCfg
  final class FieldRelabelled[FromName <: String, ToName <: String, Cfg <: TransformerCfg] extends TransformerCfg
  final class CoproductInstance[InstType, TargetType, Cfg <: TransformerCfg] extends TransformerCfg
  final class CoproductInstancePartial[InstType, TargetType, Cfg <: TransformerCfg] extends TransformerCfg
}
