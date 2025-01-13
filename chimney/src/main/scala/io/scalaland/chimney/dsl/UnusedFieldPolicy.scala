package io.scalaland.chimney.dsl

// TODO: documentation
sealed abstract class UnusedFieldPolicy
case object FailOnIgnoredSourceVal extends UnusedFieldPolicy
