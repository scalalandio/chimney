package io.scalaland.chimney.internal

sealed abstract class PatcherCfg

object PatcherCfg {
  final class Empty extends PatcherCfg
  final class IgnoreRedundantPatcherFields[C <: PatcherCfg] extends PatcherCfg
  final class IgnoreNoneInPatch[C <: PatcherCfg] extends PatcherCfg
}
