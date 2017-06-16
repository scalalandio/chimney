package io.scalaland.chimney

import io.scalaland.chimney.internal.PatcherInstances


trait Patcher[T, P] {
  def patch(obj: T, patch: P): T
}

object Patcher extends PatcherInstances
