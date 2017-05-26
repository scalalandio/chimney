package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}
import shapeless.HNil


class ValueProviderSpec extends WordSpec with MustMatchers {

  "ValueProvider" should {

    case class Source(foo: String)

    "provide value for field with no modifiers" in {

      ValueProvider(Source("test"), 'foo, classOf[String], HNil) mustBe "test"
    }

    "provide value for field with field constant modifier" in {

      ValueProvider(Source("test"), 'foo, classOf[String],
        Modifier.fieldConstant[Source, String]('foo, "provided") :: HNil) mustBe
        "provided"
    }

    "provide value for field with field function modifier" in {

      ValueProvider(Source("test"), 'foo, classOf[String],
        Modifier.fieldFunction[Source, String]('foo, _.foo * 2) :: HNil) mustBe
        "testtest"
    }

    "provide value for field with relabelling modifier" in {

      ValueProvider(Source("test"), 'bar, classOf[String],
        Modifier.relabel('foo, 'bar) :: HNil) mustBe
        "test"
    }

    "pick applicable modifier" in {

      ValueProvider(Source("test"), 'foo, classOf[String],
        Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
          Modifier.fieldConstant[Source, String]('foo, "provided") ::
          Modifier.fieldConstant[Source, String]('na, "non-applicable") :: HNil) mustBe
        "provided"
    }

    "pick first of two applicable modifiers" in {

      ValueProvider(Source("test"), 'foo, classOf[String],
        Modifier.fieldConstant[Source, String]('foo, "provided1") ::
          Modifier.fieldConstant[Source, String]('foo, "provided2") :: HNil) mustBe
        "provided1"
    }
  }
}
