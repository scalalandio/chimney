package io.scalaland.chimney.integrations

import io.scalaland.chimney.partial

trait PartialOuterTransformer[From, To, InnerFrom, InnerTo] {

  def transformWithTotalInner(
      src: From,
      failFast: Boolean,
      inner: InnerFrom => InnerTo
  ): partial.Result[To]

  def transformWithPartialInner(
      src: From,
      failFast: Boolean,
      inner: InnerFrom => partial.Result[InnerTo]
  ): partial.Result[To]
}
