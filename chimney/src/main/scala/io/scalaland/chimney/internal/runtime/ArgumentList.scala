package io.scalaland.chimney.internal.runtime

sealed abstract class ArgumentList
object ArgumentList {
  final class Empty extends ArgumentList
  final class Argument[Name <: String, Type, Args <: ArgumentList] extends ArgumentList
}
