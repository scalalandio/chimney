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

    "fix issue #105" - {

      case class Foo(a: String, b: Int, c: Int)

      "fix 'wrong forward definition' when defining implicit transformer" - {
        case class Bar(a: String, b: Int, x: Long)

        implicit val fooBarTransformer: Transformer[Foo, Bar] =
          Transformer
            .define[Foo, Bar]
            .withFieldComputed(_.x, _.c.toLong * 2)
            .buildTransformer

        Foo("a", 1, 3).transformInto[Bar] ==> Bar("a", 1, 6)
      }

      "fix stack overflow when defining implicit transformer wrapped in object" - {
        case class Bar(a: String, b: Int, x: Long)

        object TransformerInstances {
          implicit val fooBarTransformer: Transformer[Foo, Bar] =
            Transformer
              .define[Foo, Bar]
              .withFieldComputed(_.x, _.c.toLong * 2)
              .buildTransformer

        }

        import TransformerInstances._

        Foo("a", 1, 3).transformInto[Bar] ==> Bar("a", 1, 6)
        Foo("a", 1, 3).transformInto[Bar](fooBarTransformer) ==> Bar("a", 1, 6)
      }

      "fix 'wrong forward reference' when assigning .derive to local transformer instance" - {
        case class Bar(a: String, b: Int)

        implicit val fooBarTransformer: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]

        Foo("a", 1, 3).transformInto[Bar] ==> Bar("a", 1)
      }

      "fix stack overflow when assigning .derive to local transformer instance wrapped in object" - {
        case class Bar(a: String, b: Int)

        object TransformerInstances {
          implicit val fooBarTransformer: Transformer[Foo, Bar] =
            Transformer.derive[Foo, Bar]
        }

        import TransformerInstances._

        Foo("a", 1, 3).transformInto[Bar] ==> Bar("a", 1)
        Foo("a", 1, 3).transformInto[Bar](fooBarTransformer) ==> Bar("a", 1)
      }
    }

    "fix issue #108" - {
      Issue108.result ==> Issue108.expected
    }

    "fix issue #113" - {
      case class Bar1(i: Int)
      case class Bar2(i: String)
      case class Bar3(i: Option[Int])

      case class Baz1(b: Bar1)
      case class Baz2(b: Option[Bar1])
      case class Baz3(b: Bar2)
      case class Baz4(b: Option[Bar2])

      implicit val intToString: Transformer[Int, String] = _.toString

      1.transformInto[String] ==> "1"
      Option(1).into[Int].enableUnsafeOption.transform ==> 1
      Option(1).into[String].enableUnsafeOption.transform ==> "1"
      Bar1(1).transformInto[Bar2] ==> Bar2("1")
      Option(Bar1(1)).into[Bar1].enableUnsafeOption.transform ==> Bar1(1)
      Baz2(Option(Bar1(1))).into[Baz1].enableUnsafeOption.transform ==> Baz1(Bar1(1))
      Option(Bar1(1)).into[Bar2].enableUnsafeOption.transform ==> Bar2("1")
      Baz2(Option(Bar1(1))).into[Baz4].enableUnsafeOption.transform ==> Baz4(Option(Bar2("1")))
      Bar3(Option(1)).into[Bar2].enableUnsafeOption.transform ==> Bar2("1")
    }

    "fix issue #121" - {
      case class FooNested(num: Option[Int])
      case class Foo(maybeString: Option[Set[String]], nested: FooNested)

      case class BarNested(num: String)
      case class Bar(maybeString: scala.collection.immutable.Seq[String], nested: BarNested)

      val foo = Foo(None, FooNested(None))

      compileError("foo.into[Bar].transform")
        .check(
          "",
          "derivation from foo.maybeString: scala.Option to scala.collection.immutable.Seq is not supported in Chimney!",
          "derivation from foo.nested.num: scala.Option to java.lang.String is not supported in Chimney!"
        )
    }

    "fix issue #125" - {
      case class Strings(elems: Set[String])
      case class Lengths(elems: Seq[Int])

      implicit def lengthTranformer = new Transformer[String, Int] {
        override def transform(string: String): Int = string.length
      }

      val inputStrings = Strings(Set("one", "two", "three"))
      val lengths = inputStrings.into[Lengths].transform
      lengths.elems.size ==> 3
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
