package io.scalaland.chimney

trait TransformerComposition[OutFrom, OutTo] {
  type InFrom
  type InTo

  def apply(inner: InFrom => InTo): Transformer[OutFrom, OutTo]
}

object TransformerComposition {
  type Aux[InFrom0, InTo0, OutFrom, OutTo] = TransformerComposition[OutFrom, OutTo] {
    type InFrom = InFrom0
    type InTo = InTo0
  }

  def build[InFrom0, InTo0, OutFrom, OutTo](
      func: OutFrom => Transformer[InFrom0, InTo0] => OutTo
  ): TransformerComposition.Aux[InFrom0, InTo0, OutFrom, OutTo] =
    new TransformerComposition[OutFrom, OutTo] {
      type InFrom = InFrom0
      type InTo = InTo0

      def apply(inner: InFrom => InTo): Transformer[OutFrom, OutTo] =
        func(_)((src: InFrom) => inner(src))
    }
}
