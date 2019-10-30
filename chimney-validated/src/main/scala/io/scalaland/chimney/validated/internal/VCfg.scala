package io.scalaland.chimney.validated.internal

sealed abstract class VCfg

final class VEmpty extends VCfg
final class FieldConstV[Name <: String, C <: VCfg] extends VCfg
final class FieldComputedV[Name <: String, C <: VCfg] extends VCfg
