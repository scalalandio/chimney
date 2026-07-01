package io.scalaland.chimney.internal

import hearth.fp.effect.{MErrors, MIO}

/** Hearth-based port of the `io.scalaland.chimney.internal.compiletime` result/error kernel.
  *
  * Differences vs the old package (documented in detail in [[compiletime2.DerivationResult]]):
  *   - `DerivationResult[A]` is now a type alias for Hearth's [[hearth.fp.effect.MIO]] - the old hand-rolled
  *     journal+errors monad is gone,
  *   - `DerivationErrors` is now a type alias for [[hearth.fp.effect.MErrors]] (`NonEmptyVector[Throwable]`) - the
  *     error ADTs ([[compiletime2.DerivationError]]) are reparented on a stackless `Exception` so they can travel
  *     inside it; `prettyPrint` and the old result combinators are provided as syntax by [[compiletime2.ResultSyntax]]
  *     (mixed into `ChimneyDefinitions`, so the whole cake sees them without imports - NB package-object implicits
  *     would NOT be visible in nested packages, verified experimentally).
  */
package object compiletime2 {

  private[compiletime2] type DerivationResult[+A] = MIO[A]

  private[compiletime2] type DerivationErrors = MErrors
}
