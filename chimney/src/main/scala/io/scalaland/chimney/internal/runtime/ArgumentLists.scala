package io.scalaland.chimney.internal.runtime

sealed abstract class ArgumentLists
object ArgumentLists {
  final class Empty extends ArgumentLists
  final class List[Head <: ArgumentList, Tail <: ArgumentLists] extends ArgumentLists
}
