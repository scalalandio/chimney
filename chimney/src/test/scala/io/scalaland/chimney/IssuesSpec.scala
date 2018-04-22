package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}

class IssuesSpec extends WordSpec with MustMatchers {

  import dsl._

  "IssuesSpec" should {

    "fix issue #19" in {
      case class NewEntity(name: String)
      case class Entity(id: Long, name: String, isDeleted: Boolean)

      NewEntity("name")
        .into[Entity]
        .withFieldConst(_.id, 0L)
        .withFieldConst(_.isDeleted, false)
        .transform mustBe
        Entity(0, "name", isDeleted = false)
    }

    "fix issue #21" in {
      import shapeless.tag
      import shapeless.tag._
      sealed trait Test

      case class EntityWithTag1(id: Long, name: String @@ Test)
      case class EntityWithTag2(name: String @@ Test)

      EntityWithTag1(0L, tag[Test]("name")).transformInto[EntityWithTag2] mustBe EntityWithTag2(tag[Test]("name"))
    }

    "fix issue #40" in {

      case class One(text: Option[String])
      case class Two(text: Option[String])

      One(None).transformInto[Two] mustBe Two(None)
      One(Some("abc")).transformInto[Two] mustBe Two(Some("abc"))
    }

    "fix issue #44" in {

      implicit val vcTransformer: Transformer[VC, String] = _ => "abc"
      VC("test").transformInto[String] mustBe "abc"
    }

    "fix issue #46" in {
      case class X(a: Int)
      case class Y(a: Int, b: Option[String])

      X(5).into[Y].withFieldComputed(_.b, _ => Some("5")).transform
      X(5).into[Y].withFieldComputed(_.b, _ => None).transform

      case class Y2(a: Int, b: List[String])

      X(5).into[Y2].withFieldComputed(_.b, _ => Nil).transform
      X(5).into[Y2].withFieldConst(_.b, "a" :: Nil).transform
    }
  }
}

case class VC(x: String) extends AnyVal
