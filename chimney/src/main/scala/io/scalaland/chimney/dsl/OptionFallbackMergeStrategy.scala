package io.scalaland.chimney.dsl

// TODO: documentation
sealed abstract class OptionFallbackMergeStrategy
case object SourceOrElseFallback extends OptionFallbackMergeStrategy
case object FallbackOrElseSource extends OptionFallbackMergeStrategy
