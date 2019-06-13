package io.scalaland.chimney

import utest._

object IssuesSpec extends TestSuite {

  import dsl._

  // Compilation fails when moved inside the Tests block
  object Issue108 {
    case class Foo(i: FooA)
    sealed trait FooA extends Product with Serializable
    object FooA {
      case object A0 extends FooA
    }

    case class Bar(i: BarA)
    sealed trait BarA extends Product with Serializable
    object BarA {
      case object A0 extends BarA
    }

    val result: Bar = Foo(FooA.A0).transformInto[Bar]
    val expected: Bar = Bar(BarA.A0)
  }


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

      X(5).into[Y].withFieldComputed(_.b, _ => Some("5")).transform ==> Y(5, Some("5"))
      X(5).into[Y].withFieldComputed(_.b, _ => None).transform ==> Y(5, None)

      case class Y2(a: Int, b: List[String])

      X(5).into[Y2].withFieldComputed(_.b, _ => Nil).transform ==> Y2(5, Nil)
      X(5).into[Y2].withFieldConst(_.b, "a" :: Nil).transform ==> Y2(5, List("a"))
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
          """)
          .check("", "Value passed to `withFieldConst` is of type: String")
      }

      "fix for `withFieldComputed`" - {

        compileError("""
          Foo1("test")
            .into[Foo2]
            .withFieldComputed(_.x, _ => "xyz")
        """)
          .check("", "Function passed to `withFieldComputed` returns type: String")
      }

      "fix for `withFieldRenamed`" - {

        assert(
          Foo1("test")
            .into[Foo3]
            .withFieldRenamed(_.y, _.x) != null
        )
      }
    }

    "fix issue #94" - {

      case class Foo1(x: Int)
      case class Foo2(x: Option[Int])

      Foo1(5).transformInto[Foo2] ==> Foo2(Some(5))
    }

    "fix issue #101" - {

      case class Foo(`a.b`: String)
      case class Bar(b: String)

      import io.scalaland.chimney.dsl._

      Foo("a").into[Bar].withFieldRenamed(_.`a.b`, _.b).transform
    }

    "fix issue #108" - {
      Issue108.result ==> Issue108.expected
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
