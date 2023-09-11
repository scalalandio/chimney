package io.scalaland.chimney.internal.runtime

sealed abstract class PatcherFlags
object PatcherFlags {
  final class Default extends PatcherFlags
  final class Enable[F <: Flag, Flags <: PatcherFlags] extends PatcherFlags
  final class Disable[F <: Flag, Flags <: PatcherFlags] extends PatcherFlags

  sealed abstract class Flag
  final class IgnoreRedundantPatcherFields extends Flag
  final class IgnoreNoneInPatch extends Flag
  final class MacrosLogging extends Flag
}
