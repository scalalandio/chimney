package io.scalaland.chimney

class TotalTransformerSpec extends ChimneySpec {

  test("implicit summon") {
    implicit val stringTransformer = new Transformer[String, Int] {
      override def transform(src: String): Int = src.length
    }

    Transformer[String, Int].transform("foo") ==> 3
  }
}
