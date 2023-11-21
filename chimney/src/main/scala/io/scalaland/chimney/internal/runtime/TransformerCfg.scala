package io.scalaland.chimney.internal.runtime

sealed abstract class TransformerCfg
object TransformerCfg {
  final class Empty extends TransformerCfg
  final class FieldConst[Field <: Path, Cfg <: TransformerCfg] extends TransformerCfg
  final class FieldConstPartial[Field <: Path, Cfg <: TransformerCfg] extends TransformerCfg
  final class FieldComputed[Field <: Path, Cfg <: TransformerCfg] extends TransformerCfg
  final class FieldComputedPartial[Field <: Path, Cfg <: TransformerCfg] extends TransformerCfg
  final class FieldRelabelled[FromField <: Path, ToField <: Path, Cfg <: TransformerCfg] extends TransformerCfg
  final class CoproductInstance[InstType, TargetType, Cfg <: TransformerCfg] extends TransformerCfg
  final class CoproductInstancePartial[InstType, TargetType, Cfg <: TransformerCfg] extends TransformerCfg
  final class Constructor[Args <: ArgumentLists, TargetType, Cfg <: TransformerCfg] extends TransformerCfg
  final class ConstructorPartial[Args <: ArgumentLists, TargetType, Cfg <: TransformerCfg] extends TransformerCfg
}
