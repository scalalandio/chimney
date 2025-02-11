package io.scalaland.chimney.integrations

import io.scalaland.chimney.partial

/** Tells Chimney how to convert some outer types/wrappers/collections where conversions for their inner values/items
  * could be derived, when the type sometimes cannot be constructed.
  *
  * @tparam From
  *   whole outer source type (with all possible type parameters applied)
  * @tparam To
  *   whole outer target type (with all possible type parameters applied)
  * @tparam InnerFrom
  *   type of the value(s) inside From that Chimney can derive conversion from
  * @tparam InnerTo
  *   type of the value(s) inside To that Chimney can derive conversion to
  *
  * @since 1.5.0
  */
trait PartialOuterTransformer[From, To, InnerFrom, InnerTo] {

  /** Converts the outer type when the conversion of inner types turn out to be total. */
  def transformWithTotalInner(
      src: From,
      failFast: Boolean,
      inner: InnerFrom => InnerTo
  ): partial.Result[To]

  /** Converts the outer type when the conversion of inner types turn out to be partial. */
  def transformWithPartialInner(
      src: From,
      failFast: Boolean,
      inner: InnerFrom => partial.Result[InnerTo]
  ): partial.Result[To]
}
