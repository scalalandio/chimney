package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}

class IssuesSpec extends WordSpec with MustMatchers {

  import dsl._

  "IssuesSpec" should {

    "fix issue #19" in {
      case class NewEntity(name: String)
      case class Entity(id: Long, name: String, isDeleted: Boolean)

      NewEntity("name").into[Entity]
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
      case class EntityWithTag2(id: Long, name: String @@ Test)

      EntityWithTag1(0L, tag[Test]("name")).transformInto[EntityWithTag2] mustBe EntityWithTag2(0L, tag[Test]("name"))
    }
  }

}
