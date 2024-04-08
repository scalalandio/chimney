package io.scalaland.chimney.internal.runtime

sealed abstract class Path
object Path {
  final class Root extends Path
  final class Select[Init <: Path, FieldName <: String] extends Path
  final class Matching[Init <: Path, Subtype] extends Path
  final class EveryItem[Init <: Path] extends Path
  final class EveryMapKey[Init <: Path] extends Path
  final class EveryMapValue[Init <: Path] extends Path
}
