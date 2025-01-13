package io.scalaland.chimney.dsl

// TODO: documentation
sealed abstract class CollectionFallbackMergeStrategy
case object SourceAppendFallback extends CollectionFallbackMergeStrategy
case object FallbackAppendSource extends CollectionFallbackMergeStrategy
