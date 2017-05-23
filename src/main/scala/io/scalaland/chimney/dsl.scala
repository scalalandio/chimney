package io.scalaland.chimney

object dsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    def transformer[To]: TransformTo[From, To] = new TransformTo(source)
  }

  class TransformTo[From, To](val source: From) extends AnyVal {

    def invoke(implicit transformer: Transformer.Aux[From, To]): To =
      transformer.transform(source, Modifier.empty)

    def invokeM[M <: Modifier](modifier: M)
                              (implicit transformer: Transformer[From, To, M]): To =
      transformer.transform(source, modifier)
  }

  type Modifier = io.scalaland.chimney.Modifier
  val Modifier = io.scalaland.chimney.Modifier


}
