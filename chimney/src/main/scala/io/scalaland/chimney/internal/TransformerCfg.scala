package io.scalaland.chimney.internal

sealed abstract class TransformerCfg
object TransformerCfg {
  final class Empty extends TransformerCfg
  final class FieldConst[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldConstF[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldComputed[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldComputedF[Name <: String, C <: TransformerCfg] extends TransformerCfg
  final class FieldRelabelled[FromName <: String, ToName <: String, C <: TransformerCfg] extends TransformerCfg
  final class CoproductInstance[InstType, TargetType, C <: TransformerCfg] extends TransformerCfg
  final class CoproductInstanceF[InstType, TargetType, C <: TransformerCfg] extends TransformerCfg
  final class WrapperType[F[+_], C <: TransformerCfg] extends TransformerCfg
}
