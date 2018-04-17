package io.scalaland.chimney.internal

sealed abstract class Lst
final class Cns[H, T <: Lst] extends Lst
final class Nl extends Lst

final class DisableDefaults
final class FieldConst[Name <: String]
final class FieldComputed[Name <: String]
