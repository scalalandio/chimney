package io.chimney


object dsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    def to[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }

}
