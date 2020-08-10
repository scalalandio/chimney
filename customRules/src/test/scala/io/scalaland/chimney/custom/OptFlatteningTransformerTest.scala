package io.scalaland.chimney.custom

import utest._
import OptFlatteningTransformerDerivation._
import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl._

object OptFlatteningTransformerTest extends TestSuite {
  val tests = Tests {
    "support of opt rule" - {
      case class ClassA(a: Option[Seq[Int]])
      case class ClassB(a: Seq[String])

      implicit val intToString: Transformer[Int, String] = _.toString

      ClassA(Some(Seq.empty)).transformInto[ClassB] ==> ClassB(Seq.empty)

      ClassA(Some(Seq(1))).transformInto[ClassB] ==> ClassB(Seq("1"))
    }
  }
}
