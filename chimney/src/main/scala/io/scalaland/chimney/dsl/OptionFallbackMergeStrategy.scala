package io.scalaland.chimney.dsl

/** What should happen when both source value and fallback values are of the Option/Either type.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#merging-option-with-option-into-option]] for more
  *   details
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#merging-either-with-either-into-either]] for more
  *   details
  *
  * @since 1.7.0
  */
sealed abstract class OptionFallbackMergeStrategy

/** When both source value and fallback values are of the Option/Either type they should be combined using
  * `src.orElse(fallback1).orElse(fallback2)...`.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#merging-option-with-option-into-option]] for more
  *   details
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#merging-either-with-either-into-either]] for more
  *   details
  *
  * @since 1.7.0
  */
case object SourceOrElseFallback extends OptionFallbackMergeStrategy

/** When both source value and fallback values are of the Option/Either type they should be combined using
  * `fallbck2.orElse(fallback1).orElse(src)`.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#merging-option-with-option-into-option]] for more
  *   details
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#merging-either-with-either-into-either]] for more
  *   details
  *
  * @since 1.7.0
  */
case object FallbackOrElseSource extends OptionFallbackMergeStrategy
