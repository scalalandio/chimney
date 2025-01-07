package io.scalaland.chimney.dsl

sealed abstract class UnusedFieldPolicy
case object FailOnIgnoredSourceVal extends UnusedFieldPolicy
