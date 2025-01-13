package io.scalaland.chimney.dsl

// TODO: documentation
sealed abstract class UnmatchedSubtypePolicy
case object FailOnUnmatchedTargetSubtype extends UnmatchedSubtypePolicy
