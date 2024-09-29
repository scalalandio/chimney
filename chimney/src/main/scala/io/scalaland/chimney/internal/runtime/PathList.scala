package io.scalaland.chimney.internal.runtime

sealed abstract class PathList
object PathList {
  final class Empty extends PathList
  final class List[Head <: Path, Tail <: PathList] extends PathList
}
