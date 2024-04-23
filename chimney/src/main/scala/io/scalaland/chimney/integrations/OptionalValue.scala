package io.scalaland.chimney.integrations

/** Tells Chimney how to interact with `Optional` type to align its behavior with [[Option]] of `Value`.
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#custom-optional-types]] for more details
  *
  * @tparam Optional
  *   type storing optional `Value` - has to be proper type, not higher-kinded type
  * @tparam Value
  *   type of internal value (if present)
  *
  * @since 1.0.0
  */
trait OptionalValue[Optional, Value] {

  /** Creates an empty optional value. */
  def empty: Optional

  /** Creates non-empty optional value (should handle nulls as empty). */
  def of(value: Value): Optional

  /** Folds optional value just like [[Option.fold]]. */
  def fold[A](oa: Optional, onNone: => A, onSome: Value => A): A

  /** Should work like [[Option.getOrElse]]. */
  def getOrElse(oa: Optional, onNone: => Value): Value = fold(oa, onNone, identity)

  /** Should work like [[Option.orElse]]. */
  def orElse(oa: Optional, onNone: => Optional): Optional = fold(oa, onNone, _ => oa)
}
