package io.scalaland.chimney.internal.runtime

sealed abstract class Path
object Path {
  final class Root extends Path
  final class Select[Name <: String, Instance <: Path] extends Path
}
