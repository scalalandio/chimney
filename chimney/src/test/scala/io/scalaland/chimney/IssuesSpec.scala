package io.scalaland.chimney

import utest._

object IssuesSpec extends TestSuite {

  import dsl._

  val tests = Tests {

    "fix issue #19" - {
      case class NewEntity(name: String)
      case class Entity(id: Long, name: String, isDeleted: Boolean)

      NewEntity("name")
        .into[Entity]
        .withFieldConst(_.id, 0L)
        .withFieldConst(_.isDeleted, false)
        .transform ==>
        Entity(0, "name", isDeleted = false)
    }

    "fix issue #21" - {
      import tag._
      sealed trait Test

      case class EntityWithTag1(id: Long, name: String @@ Test)
      case class EntityWithTag2(name: String @@ Test)

      EntityWithTag1(0L, tag[Test]("name")).transformInto[EntityWithTag2] ==>
        EntityWithTag2(tag[Test]("name"))
    }

    "fix issue #40" - {

      case class One(text: Option[String])
      case class Two(text: Option[String])

      One(None).transformInto[Two] ==> Two(None)
      One(Some("abc")).transformInto[Two] ==> Two(Some("abc"))
    }

    "fix issue #44" - {

      implicit val vcTransformer: Transformer[VC, String] = _ => "abc"
      VC("test").transformInto[String] ==> "abc"
    }

    "fix issue #46" - {
      case class X(a: Int)
      case class Y(a: Int, b: Option[String])

      X(5).into[Y].withFieldComputed(_.b, _ => Some("5")).transform
      X(5).into[Y].withFieldComputed(_.b, _ => None).transform

      case class Y2(a: Int, b: List[String])

      X(5).into[Y2].withFieldComputed(_.b, _ => Nil).transform
      X(5).into[Y2].withFieldConst(_.b, "a" :: Nil).transform
    }

    "fix issue #66" - {

      case class Foo1(y: String)
      case class Foo2(y: String, x: Int)
      case class Foo3(x: Int)

      "fix for `withFieldConst`" - {

        compileError("""
          Foo1("test")
            .into[Foo2]
            .withFieldConst(_.x, "xyz")
          """
        )
      }

      "fix for `withFieldComputed`" - {

        compileError("""
          Foo1("test")
            .into[Foo2]
            .withFieldComputed(_.x, _ => "xyz")
        """)
      }

      "fix for `withFieldRenamed`" - {
        Foo1("test")
          .into[Foo3]
          .withFieldRenamed(_.y, _.x)
      }
    }
  }
}

case class VC(x: String) extends AnyVal

object tag {
  def apply[U] = new Tagger[U]

  trait Tagged[U]
  type @@[+T, U] = T with Tagged[U]

  class Tagger[U] {
    def apply[T](t: T): T @@ U = t.asInstanceOf[T @@ U]
  }
}
