package io.scalaland.chimney

object dsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    def transformTo[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }

}
