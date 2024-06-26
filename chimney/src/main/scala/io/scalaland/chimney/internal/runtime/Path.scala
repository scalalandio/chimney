package io.scalaland.chimney.internal.runtime

/** Represents the path passed into `withField*` methods. */
sealed abstract class Path
object Path {

  /** Represents _ (anonymous argument) path. */
  final class Root extends Path

  /** Represents $init.$fieldName path. */
  final class Select[Init <: Path, FieldName <: String] extends Path

  /** Represents $init.matching[$Subtype] path BUT ALSO is used for `matchingSome`, `matchingLeft` and `matchingRight`.
    *
    * Currently there is some special casing - `.matchingSome`, `.matchingLeft` and `.matchingRight` not only matches
    * the subtype but also extracts the inner value AND running `_.option.matching[Some[A]].value` would NOT work.
    */
  final class Matching[Init <: Path, Subtype] extends Path

  /** Used only for withSealedSubtypeMatching/Renamed to denote that type is matched on the source side!!! */
  final class SourceMatching[Init <: Path, SourceSubtype] extends Path

  /** Represents $init.everyItem path. */
  final class EveryItem[Init <: Path] extends Path

  /** Represents $init.everyMapKey path. */
  final class EveryMapKey[Init <: Path] extends Path

  /** Represents $init.everyMapValue path. */
  final class EveryMapValue[Init <: Path] extends Path
}
