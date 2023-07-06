package io.scalaland.chimney.internal.runtime

sealed abstract class PatcherCfg
object PatcherCfg {
  final class Empty extends PatcherCfg
  final class IgnoreRedundantPatcherFields[Cfg <: PatcherCfg] extends PatcherCfg
  final class IgnoreNoneInPatch[Cfg <: PatcherCfg] extends PatcherCfg
  final class MacrosLogging[Cfg <: PatcherCfg] extends PatcherCfg
}
