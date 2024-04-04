package io.scalaland.chimney.internal.runtime

sealed abstract class Path
object Path {
  final class Root extends Path
  final class Select[Init <: Path, FieldName <: String] extends Path
  final class Match[Init <: Path, Subtype] extends Path
}
