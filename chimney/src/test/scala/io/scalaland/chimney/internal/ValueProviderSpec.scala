package io.scalaland.chimney.internal

import org.scalatest.{MustMatchers, WordSpec}
import shapeless.HNil

class ValueProviderSpec extends WordSpec with MustMatchers {

  "ValueProvider" should {

    case class Source(foo: String)

    "provide value for field with no modifiers" in {

      ValueProvider.provide(Source("test"), 'foo, classOf[String], HNil) mustBe "test"
    }

    "provide value for field with field constant modifier" in {

      ValueProvider.provide(
        Source("test"),
        'foo,
        classOf[String],
        Modifier.fieldConstant[Source, String]('foo, "provided") :: HNil
      ) mustBe
        "provided"
    }

    "provide value for field with field function modifier" in {

      ValueProvider.provide(
        Source("test"),
        'foo,
        classOf[String],
        Modifier.fieldFunction[Source, String]('foo, _.foo * 2) :: HNil
      ) mustBe
        "testtest"
    }

    "provide value for field with relabelling modifier" in {

      ValueProvider.provide(Source("test"), 'bar, classOf[String], Modifier.relabel('foo, 'bar) :: HNil) mustBe
        "test"
    }

    "pick applicable modifier" when {

      "applicable is first" in {
        ValueProvider.provide(
          Source("test"),
          'foo,
          classOf[String],
          Modifier.fieldConstant[Source, String]('foo, "provided") ::
            Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
            Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
            HNil
        ) mustBe
          "provided"
      }

      "applicable is in the middle" in {
        ValueProvider.provide(
          Source("test"),
          'foo,
          classOf[String],
          Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
            Modifier.fieldConstant[Source, String]('foo, "provided") ::
            Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
            HNil
        ) mustBe
          "provided"
      }

      "applicable is last" in {
        ValueProvider.provide(
          Source("test"),
          'foo,
          classOf[String],
          Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
            Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
            Modifier.fieldConstant[Source, String]('foo, "provided") ::
            HNil
        ) mustBe
          "provided"
      }
    }

    "pick first of applicable modifiers" in {

      ValueProvider.provide(
        Source("test"),
        'foo,
        classOf[String],
        Modifier.fieldConstant[Source, String]('foo, "provided1") ::
          Modifier.fieldConstant[Source, String]('foo, "provided2") ::
          Modifier.fieldConstant[Source, String]('foo, "provided3") ::
          HNil
      ) mustBe
        "provided1"
    }

    "provide value for coproduct field" in {

      ValueProvider.provide(Response(No), 'answer, classOf[Answer], HNil) mustBe
        No

      ValueProvider.provide(Response(Yes), 'answer, classOf[Answer], HNil) mustBe
        Yes

      ValueProvider.provide(Response2("no", No), 'answer, classOf[Answer], HNil) mustBe
        No

      ValueProvider.provide(Response2("si", Yes), 'answer, classOf[Answer], HNil) mustBe
        Yes

      ValueProvider.provide(Response2("no", No), 'other, classOf[String], HNil) mustBe
        "no"

      ValueProvider.provide(Response2("si", Yes), 'other, classOf[String], HNil) mustBe
        "si"
    }
  }
}

sealed trait Answer
case object Yes extends Answer
case object No extends Answer
case class Response(answer: Answer)
case class Response2(other: String, answer: Answer)
