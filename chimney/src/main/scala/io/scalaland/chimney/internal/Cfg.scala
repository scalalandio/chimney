package io.scalaland.chimney.internal

sealed abstract class Cfg

final class Empty extends Cfg
final class DisableDefaults[C <: Cfg] extends Cfg
final class FieldConst[Name <: String, C <: Cfg] extends Cfg
final class FieldComputed[Name <: String, C <: Cfg] extends Cfg
final class FieldRelabelled[FromName <: String, ToName <: String, C <: Cfg] extends Cfg
final class CoproductInstance[InstType, TargetType, C <: Cfg] extends Cfg
