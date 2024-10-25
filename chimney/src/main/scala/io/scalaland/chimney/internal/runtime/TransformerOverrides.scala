package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.internal.runtime.TransformerOverrides as Overrides

sealed abstract class TransformerOverrides
object TransformerOverrides {
  final class Empty extends Overrides
  // Provides a precomputed value
  final class Const[ToPath <: Path, Tail <: Overrides] extends Overrides
  final class ConstPartial[ToPath <: Path, Tail <: Overrides] extends Overrides
  // Computes a value from a whole (src: From)
  final class Computed[ToPath <: Path, Tail <: Overrides] extends Overrides
  final class ComputedPartial[ToPath <: Path, Tail <: Overrides] extends Overrides
  // Computes a value from an already extracted (e.g. matched) piece of (src: From)
  final class CaseComputed[ToPath <: Path, Tail <: Overrides] extends Overrides
  final class CaseComputedPartial[ToPath <: Path, Tail <: Overrides] extends Overrides
  // Computes a value after all constructor arguments have been matched
  final class Constructor[Args <: ArgumentLists, ToPath <: Path, Tail <: Overrides] extends Overrides
  final class ConstructorPartial[Args <: ArgumentLists, ToPath <: Path, Tail <: Overrides] extends Overrides
  // Computes a value using manually pointed value from (src: From)
  final class RenamedFrom[FromPath <: Path, ToPath <: Path, Tail <: Overrides] extends Overrides
  // Computes a value from matched subtype, targeting another subtype
  final class RenamedTo[FromPath <: Path, ToPath <: Path, Tail <: Overrides] extends Overrides
  // Fallback value allowing merging several sources
  final class Fallback[FromFallback, ToPath <: Path, Tail <: Overrides] extends Overrides
}
