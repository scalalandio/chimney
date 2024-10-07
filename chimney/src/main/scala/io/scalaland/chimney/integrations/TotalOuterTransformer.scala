package io.scalaland.chimney.integrations

import io.scalaland.chimney.partial

trait TotalOuterTransformer[From, To, InnerFrom, InnerTo] {

  def transformWithTotalInner(
      src: From,
      inner: InnerFrom => InnerTo
  ): To

  def transformWithPartialInner(
      src: From,
      inner: InnerFrom => partial.Result[InnerTo]
  ): partial.Result[To]
}
