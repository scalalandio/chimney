package io.scalaland.chimney.dsl

/** What should happen when both source value and fallback values are collections.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#merging-collection-with-collection-into-collection]]
  *   for more details
  *
  * @since 1.7.0
  */
sealed abstract class CollectionFallbackMergeStrategy

/** When both source value and fallback values are collections they should be combined using
  * `src ++ fallback1 ++ fallback2 ...`.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#merging-collection-with-collection-into-collection]]
  *   for more details
  *
  * @since 1.7.0
  */
case object SourceAppendFallback extends CollectionFallbackMergeStrategy

/** When both source value and fallback values are collections they should be combined using
  * `fallback2 ++ fallback1 ++ src`.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#merging-collection-with-collection-into-collection]]
  *   for more details
  *
  * @since 1.7.0
  */
case object FallbackAppendSource extends CollectionFallbackMergeStrategy
