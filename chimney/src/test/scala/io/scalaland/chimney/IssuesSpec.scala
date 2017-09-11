package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}
import shapeless.HNil

class IssuesSpec extends WordSpec with MustMatchers {

  import dsl._

  "IssuesSpec" should {

    "fix issue #19" in {
      case class NewEntity(name: String)
      case class Entity(id: Long, name: String, isDeleted: Boolean)

      NewEntity("name")
        .into[Entity]
        .withFieldConst('id, 0L)
        .withFieldConst('isDeleted, false)
        .transform mustBe
        Entity(0, "name", isDeleted = false)
    }

    "fix issue #21" in {
      import shapeless.tag
      import shapeless.tag._
      sealed trait Test

      case class EntityWithTag1(id: Long, name: String @@ Test)
      case class EntityWithTag2(name: String @@ Test)

      // This test doesn't work on 2.12+ due to:
      // https://github.com/milessabin/shapeless/pull/726
      // EntityWithTag1(0L, tag[Test]("name")).transformInto[EntityWithTag2] mustBe EntityWithTag2(tag[Test]("name"))
    }

    "fix issue #40" in {

      case class One(text: Option[String])
      case class Two(text: Option[String])

      One(None).transformInto[Two] mustBe Two(None)
      One(Some("abc")).transformInto[Two] mustBe Two(Some("abc"))
    }
  }

}
