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
  }

}
