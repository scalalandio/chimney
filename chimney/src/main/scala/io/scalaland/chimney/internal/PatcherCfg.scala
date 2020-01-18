package io.scalaland.chimney.internal

sealed abstract class PatcherCfg

object PatcherCfg {
  final class Empty extends PatcherCfg
  final class EnableIncompletePatches[C <: PatcherCfg] extends PatcherCfg
}
