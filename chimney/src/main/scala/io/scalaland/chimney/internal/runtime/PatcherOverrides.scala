package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.internal.runtime.PatcherOverrides as Overrides

sealed abstract class PatcherOverrides
object PatcherOverrides {
  final class Empty extends Overrides
}
