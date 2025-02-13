package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.internal.runtime.TransformerOverrides as Overrides

sealed abstract class TransformerOverrides
object TransformerOverrides {
  final class Empty extends Overrides
  // Suppress unused field/unmatched subtype error
  final class Unused[FromPath <: Path, Tail <: Overrides] extends Overrides
  final class Unmatched[ToPath <: Path, Tail <: Overrides] extends Overrides
  // Provides a precomputed value
  final class Const[ToPath <: Path, Tail <: Overrides] extends Overrides
  final class ConstPartial[ToPath <: Path, Tail <: Overrides] extends Overrides
  // Computes a value from an expr
  final class Computed[FromPath <: Path, ToPath <: Path, Tail <: Overrides] extends Overrides
  final class ComputedPartial[FromPath <: Path, ToPath <: Path, Tail <: Overrides] extends Overrides
  // Computes a value after all constructor arguments have been matched
  final class Constructor[Args <: ArgumentLists, ToPath <: Path, Tail <: Overrides] extends Overrides
  final class ConstructorPartial[Args <: ArgumentLists, ToPath <: Path, Tail <: Overrides] extends Overrides
  // Computes a value using manually pointed value from
  final class Renamed[FromPath <: Path, ToPath <: Path, Tail <: Overrides] extends Overrides
  // Fallback value allowing merging several sources
  final class Fallback[FromFallback, ToPath <: Path, Tail <: Overrides] extends Overrides
}
