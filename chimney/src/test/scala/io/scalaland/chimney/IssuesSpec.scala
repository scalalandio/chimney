package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.annotation.unused

class IssuesSpec extends ChimneySpec {

  test("fix issue #19") {
    case class NewEntity(name: String)
    case class Entity(id: Long, name: String, isDeleted: Boolean)

    NewEntity("name")
      .into[Entity]
      .withFieldConst(_.id, 0L)
      .withFieldConst(_.isDeleted, false)
      .transform ==>
      Entity(0, "name", isDeleted = false)
  }

  test("fix issue #21") {
    import tag.*
    sealed trait Test

    case class EntityWithTag1(id: Long, name: String @@ Test)
    case class EntityWithTag2(name: String @@ Test)

    EntityWithTag1(0L, tag[Test]("name")).transformInto[EntityWithTag2] ==>
      EntityWithTag2(tag[Test]("name"))
  }

  test("fix issue #40") {

    case class One(text: Option[String])
    case class Two(text: Option[String])

    One(None).transformInto[Two] ==> Two(None)
    One(Some("abc")).transformInto[Two] ==> Two(Some("abc"))
  }

  test("fix issue #44") {

    implicit val vcTransformer: Transformer[VC, String] = _ => "abc"
    VC("test").transformInto[String] ==> "abc"
  }

  test("fix issue #46") {
    case class X(a: Int)
    case class Y(a: Int, b: Option[String])

    X(5).into[Y].withFieldComputed(_.b, _ => Some("5")).transform ==> Y(5, Some("5"))
    X(5).into[Y].withFieldComputed(_.b, _ => None).transform ==> Y(5, None)

    case class Y2(a: Int, b: List[String])

    X(5).into[Y2].withFieldComputed(_.b, _ => Nil).transform ==> Y2(5, Nil)
    X(5).into[Y2].withFieldConst(_.b, "a" :: Nil).transform ==> Y2(5, List("a"))
  }

  group("fix issue #66") {

    case class Foo1(y: String)
    @unused case class Foo2(y: String, x: Int)
    case class Foo3(x: Int)

    test("fix for `withFieldConst`") {

      compileErrors(
        """
        Foo1("test")
          .into[Foo2]
          .withFieldConst(_.x, "xyz")
        """
      ).check("Cannot prove that String <:< Int")
    }

    test("fix for `withFieldComputed`") {

      compileErrors(
        """
        Foo1("test")
          .into[Foo2]
          .withFieldComputed(_.x, _ => "xyz")
        """
      ).check("Cannot prove that String <:< Int")
    }

    test("fix for `withFieldRenamed`") {

      assert(
        Foo1("test")
          .into[Foo3]
          .withFieldRenamed(_.y, _.x) != null
      )
    }
  }

  test("fix issue #94") {

    case class Foo1(x: Int)
    case class Foo2(x: Option[Int])

    Foo1(5).transformInto[Foo2] ==> Foo2(Some(5))
  }

  test("fix issue #101") {

    case class Foo(`a.b`: String)
    case class Bar(b: String)

    import io.scalaland.chimney.dsl.*

    Foo("a").into[Bar].withFieldRenamed(_.`a.b`, _.b).transform
  }

  group("fix issue #105") {

    case class Foo(a: String, b: Int, c: Int)

    test("fix 'wrong forward definition' when defining implicit val transformer") {
      case class Bar(a: String, b: Int, x: Long)

      implicit val fooBarTransformer: Transformer[Foo, Bar] =
        Transformer
          .define[Foo, Bar]
          .withFieldComputed(_.x, _.c.toLong * 2)
          .buildTransformer

      Foo("a", 1, 3).transformInto[Bar] ==> Bar("a", 1, 6)
    }

    test("fix stack overflow when defining implicit def transformer") {
      case class Bar(a: String, b: Int, x: Long)

      implicit def fooBarTransformer: Transformer[Foo, Bar] =
        Transformer
          .define[Foo, Bar]
          .withFieldComputed(_.x, _.c.toLong * 2)
          .buildTransformer

      Foo("a", 1, 3).transformInto[Bar] ==> Bar("a", 1, 6)
    }

    test("fix stack overflow when defining implicit val transformer wrapped in object") {
      case class Bar(a: String, b: Int, x: Long)

      object TransformerInstances {
        implicit val fooBarTransformer: Transformer[Foo, Bar] =
          Transformer
            .define[Foo, Bar]
            .withFieldComputed(_.x, _.c.toLong * 2)
            .buildTransformer
      }

      import TransformerInstances.*

      Foo("a", 1, 3).transformInto[Bar] ==> Bar("a", 1, 6)
      Foo("a", 1, 3).transformInto[Bar](fooBarTransformer) ==> Bar("a", 1, 6)
    }

    test("fix 'wrong forward reference' when assigning .derive to local transformer instance") {
      case class Bar(a: String, b: Int)

      implicit val fooBarTransformer: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]

      Foo("a", 1, 3).transformInto[Bar] ==> Bar("a", 1)
    }

    test("fix stack overflow when assigning .derive to local transformer instance wrapped in object") {
      case class Bar(a: String, b: Int)

      object TransformerInstances {
        implicit val fooBarTransformer: Transformer[Foo, Bar] =
          Transformer.derive[Foo, Bar]
      }

      import TransformerInstances.*

      Foo("a", 1, 3).transformInto[Bar] ==> Bar("a", 1)
      Foo("a", 1, 3).transformInto[Bar](fooBarTransformer) ==> Bar("a", 1)
    }
  }

  test("fix issue #108") {
    import Issue108.*
    Foo(FooA.A0).transformInto[Bar] ==> Bar(BarA.A0)
  }

  test("fix issue #113 (rewritten to partials)") {
    case class Bar1(i: Int)
    case class Bar2(i: String)
    case class Bar3(i: Option[Int])

    case class Baz1(b: Bar1)
    case class Baz2(b: Option[Bar1])
    case class Baz4(b: Option[Bar2])

    implicit val intToString: Transformer[Int, String] = _.toString

    1.transformInto[String] ==> "1"
    Option(1).into[Int].partial.transform.asOption ==> Some(1)
    Option(1).into[String].partial.transform.asOption ==> Some("1")
    Bar1(1).transformInto[Bar2] ==> Bar2("1")
    Option(Bar1(1)).into[Bar1].partial.transform.asOption ==> Some(Bar1(1))
    Baz2(Option(Bar1(1))).into[Baz1].partial.transform.asOption ==> Some(Baz1(Bar1(1)))
    Option(Bar1(1)).into[Bar2].partial.transform.asOption ==> Some(Bar2("1"))
    Baz2(Option(Bar1(1))).into[Baz4].partial.transform.asOption ==> Some(Baz4(Option(Bar2("1"))))
    Bar3(Option(1)).into[Bar2].partial.transform.asOption ==> Some(Bar2("1"))
  }

  test("fix issue #121") {
    case class FooNested(num: Option[Int])
    @unused case class Foo(maybeString: Option[Set[String]], nested: FooNested)

    case class BarNested(num: String)
    @unused case class Bar(maybeString: scala.collection.immutable.Seq[String], nested: BarNested)

    compileErrors("Foo(None, FooNested(None)).into[Bar].transform")
      .check(
        "derivation from foo.maybeString: scala.Option[scala.collection.immutable.Set[java.lang.String]] to scala.collection.immutable.Seq[java.lang.String] is not supported in Chimney!",
        "derivation from foo.nested.num: scala.Option[scala.Int] to java.lang.String is not supported in Chimney!"
      )
  }

  test("fix issue #125") {
    case class Strings(elems: Set[String])
    case class Lengths(elems: Seq[Int])

    implicit def lengthTranformer: Transformer[String, Int] = new Transformer[String, Int] {
      override def transform(string: String): Int = string.length
    }

    val inputStrings = Strings(Set("one", "two", "three"))
    val lengths = inputStrings.into[Lengths].transform
    lengths.elems.size ==> 3
  }

  test("fix issue #139 (rewritten as partial)") {
    case class WithoutOption(i: Int)
    case class WithOption(i: Option[Int])

    // this should compile without warning
    Transformer.define[WithOption, WithoutOption].partial.buildTransformer
  }

  group("fix issue #149") {

    import Issue149.*

    test("example 1") {
      EntryT(EntryId(10)).patchUsing(Patch(EntryId(20))) ==> EntryT(EntryId(20))
    }

    test("example 2") {
      Real("abc").patchUsing(Data(Option("xyz"))) ==> Real("xyz")
    }

    test("example 3") {
      type Id[X] = X

      Data3(10).patchUsing(Patch3[Option](None)) ==> Data3(10)
      Data3(10).patchUsing(Patch3(Some(20))) ==> Data3(20)
      Data3(10).patchUsing(Patch3[Id](20)) ==> Data3(20)
    }
  }

  test("fix issue #156") {

    import Issue156.*

    import io.scalaland.chimney.dsl.*
    val venue = internal.ManuallyFilled("Venue Name")
    val event = internal.Event(venue)

    // Case class to case class rule, with case class param accessor
    venue.transformInto[dto.Venue] ==> dto.Venue("Venue Name")

    // These two will fail to compile as target is case class, but source type is internal.Venue,
    // thus it will try to access `def name` accessor without .enableMethodAccessors flag
    compileErrors("event.venue.transformInto[dto.Venue]").arePresent()
    compileErrors("(venue: internal.Venue).transformInto[dto.Venue]").arePresent()

    // When .enableMethodAccessors turned on, both should work fine
    event.venue.into[dto.Venue].enableMethodAccessors.transform ==> dto.Venue("Venue Name")
    (venue: internal.Venue).into[dto.Venue].enableMethodAccessors.transform ==> dto.Venue("Venue Name")
  }

  group("fix issue #168") {

    test("objects case") {
      sealed trait Version1
      case object Instance1 extends Version1
      sealed trait Version2
      case object Instance2 extends Version2

      val v1: Version1 = Instance1
      val v2: Version2 = v1
        .into[Version2]
        .withSealedSubtypeHandled { (_: Instance1.type) =>
          Instance2
        }
        .transform

      v2 ==> Instance2
    }

    test("classes case") {
      sealed trait Version1
      final case class Instance1(p: Int) extends Version1
      sealed trait Version2
      final case class Instance2(p1: Int, p2: Int) extends Version2

      val v1: Version1 = Instance1(10)
      val v2: Version2 = v1
        .into[Version2]
        .withSealedSubtypeHandled { (i: Instance1) =>
          Instance2(i.p / 2, i.p / 2)
        }
        .transform

      v2 ==> Instance2(5, 5)
    }
  }

  group("fix issue #173 (rewritten as partial)") {
    sealed trait Foo
    case object Bar extends Foo
    case object Baz extends Foo

    sealed trait Foo2
    case object Bar2 extends Foo2
    case object Baz2 extends Foo2

    test("withSealedSubtypeHandledPartial twice") {
      implicit val fooFoo2PartialTransformer: PartialTransformer[Foo, Foo2] =
        PartialTransformer
          .define[Foo, Foo2]
          .withSealedSubtypeHandledPartial((_: Bar.type) => partial.Result.fromValue(Bar2))
          .withSealedSubtypeHandledPartial((_: Baz.type) => partial.Result.fromValue(Baz2))
          .buildTransformer

      (Bar: Foo).transformIntoPartial[Foo2].asOption ==> Some(Bar2)
      (Baz: Foo).transformIntoPartial[Foo2].asOption ==> Some(Baz2)
    }

    test("withSealedSubtypeHandled followed by withSealedSubtypeHandledPartial") {
      implicit val fooFoo2PartialTransformer: PartialTransformer[Foo, Foo2] =
        PartialTransformer
          .define[Foo, Foo2]
          .withSealedSubtypeHandled((_: Bar.type) => Bar2)
          .withSealedSubtypeHandledPartial((_: Baz.type) => partial.Result.fromValue(Baz2))
          .buildTransformer

      (Bar: Foo).transformIntoPartial[Foo2].asOption ==> Some(Bar2)
      (Baz: Foo).transformIntoPartial[Foo2].asOption ==> Some(Baz2)
    }

    test("withSealedSubtypeHandledPartial followed by withSealedSubtypeHandled") {
      implicit val fooFoo2PartialTransformer: PartialTransformer[Foo, Foo2] =
        PartialTransformer
          .define[Foo, Foo2]
          .withSealedSubtypeHandledPartial((_: Bar.type) => partial.Result.fromValue(Bar2))
          .withSealedSubtypeHandled((_: Baz.type) => Baz2)
          .buildTransformer

      (Bar: Foo).transformIntoPartial[Foo2].asOption ==> Some(Bar2)
      (Baz: Foo).transformIntoPartial[Foo2].asOption ==> Some(Baz2)
    }
  }

  group("fix issue #177 (rewritten as partial)") {

    test("case 1") {
      case class Foo(x: Int)
      case class Bar(x: Int)
      case class FooW(a: Option[Foo])
      case class BarW(a: Option[Bar])

      // this should be used and shouldn't trip the unused warning
      implicit val fooToBarPartialTransformer: PartialTransformer[Foo, Bar] =
        (f, _) => partial.Result.fromValue(Bar(f.x + 10))

      FooW(Some(Foo(1))).transformIntoPartial[BarW].asOption ==> Some(BarW(Some(Bar(11))))
    }

    test("case 2") {
      case class Foo(x: Int, y: String)

      sealed abstract case class Bar(x: Int, y: String)
      object Bar {
        def make(x: Int, y: String): Bar = new Bar(x, y) {}
      }

      implicit val fooToBar: PartialTransformer[Foo, Bar] =
        (f, _) => partial.Result.fromValue(Bar.make(f.x, f.y))

      Foo(1, "test").transformIntoPartial[Bar].asOption ==> Some(new Bar(1, "test") {})
      List(Foo(1, "test")).transformIntoPartial[List[Bar]].asOption ==> Some(List(new Bar(1, "test") {}))
      (1, Foo(1, "test")).transformIntoPartial[(Int, Bar)].asOption ==> Some((1, new Bar(1, "test") {}))

      // this caused an issue - did not compile, works fine after fix
      (1, List(Foo(1, "test"))).transformIntoPartial[(Int, List[Bar])].asOption ==> Some(
        (1, List(new Bar(1, "test") {}))
      )
    }

    test("case 3") {
      case class Foo(x: Int, y: String)
      case class Bar(x: Int, y: String)
      implicit val t: PartialTransformer[Foo, Bar] =
        (f, _) => partial.Result.fromValue(Bar(f.y.length, f.x.toString)) // Swapped
      (1, List(Foo(1, "test"))).transformIntoPartial[(Int, List[Bar])].asOption ==> Some((1, List(Bar(4, "1"))))
    }
  }

  test("fix issue #185 (rewritten as partial)") {

    def blackIsRed(@unused b: colors2.Black.type): colors1.Color =
      colors1.Red

    (colors2.Black: colors2.Color)
      .intoPartial[colors1.Color]
      .withSealedSubtypeHandled(blackIsRed)
      .transform
      .asOption ==> Some(colors1.Red)

    (colors2.Red: colors2.Color)
      .intoPartial[colors1.Color]
      .withSealedSubtypeHandled(blackIsRed)
      .transform
      .asOption ==> Some(colors1.Red)

    (colors2.Green: colors2.Color)
      .intoPartial[colors1.Color]
      .withSealedSubtypeHandled(blackIsRed)
      .transform
      .asOption ==> Some(colors1.Green)

    (colors2.Blue: colors2.Color)
      .intoPartial[colors1.Color]
      .withSealedSubtypeHandled(blackIsRed)
      .transform
      .asOption ==> Some(colors1.Blue)
  }

  test("fix issue #182") {
    foo.convert(foo.A1) ==> foo.into.A1
  }

  test("fix issue #214") {

    final case class Foo(
        `Billing Zip/Postal Code`: String,
        `Shipping Zip/Postal Code`: String,
        `Billing Supplier Country (text only)`: String
    )

    final case class Bar(
        `Billing Zip/Postal Code`: String,
        `Shipping Zip/Postal Code`: String,
        `Billing Supplier Country (text only)`: String
    )

    val transformer = Transformer
      .define[Foo, Bar]
      .buildTransformer

    val foo = Foo("3152XX", "3152XX", "England")
    val expected = Bar("3152XX", "3152XX", "England")
    val result = transformer.transform(foo)
    assert(result == expected)

    val partialTransformer = Transformer
      .definePartial[Foo, Bar]
      .buildTransformer

    val partialResult = partialTransformer.transform(foo).asEither
    assert(partialResult == Right(expected))
  }

  group("fix issue #212") {

    import Issue212.*

    test("partial transformers") {
      @unused implicit val somethingPartialTransformer: PartialTransformer[proto.Something, OneOf] =
        PartialTransformer(_.value.transformIntoPartial[Something])
      @unused implicit val somethingElsePartialTransformer: PartialTransformer[proto.SomethingElse, OneOf] =
        PartialTransformer(_.value.transformIntoPartial[SomethingElse])

      implicit val oneOfPartialTransformer: PartialTransformer[proto.OneOf, OneOf] =
        PartialTransformer
          .define[proto.OneOf, OneOf]
          .withSealedSubtypeHandledPartial[proto.Empty.type](_ => partial.Result.fromErrorString("proto.OneOf.Empty"))
          .buildTransformer

      (proto.Something(proto.SomethingMessage(42)): proto.OneOf)
        .transformIntoPartial[OneOf]
        .asOption ==> Some(Something(42))

      val failedResult = (proto.Empty: proto.OneOf).transformIntoPartial[OneOf]

      failedResult.asOption ==> None
      failedResult.asErrorPathMessageStrings ==> Iterable("" -> "proto.OneOf.Empty")
    }
  }

  group("fix issue #199") {
    import Issue199.*

    test("basic sanity check") {
      A.Foo("foo").transformInto[B.Foo] ==> B.Foo("foo")
      A.Foo("foo").transformInto[C.Foo] ==> C.Foo("foo")
      A.Bar(Map("bar" -> 1)).transformInto[B.Bar] ==> B.Bar(Map("bar" -> 1))
    }

    group("with A.Bar to C.Bar transformer") {
      implicit val aBarToCBar: Transformer[A.Bar, C.Bar] = Issue199.barToBarTransformer

      test("transforming a product") {
        A.Bar(Map("bar" -> 1)).transformInto[C.Bar] ==> C.Bar(Seq("bar"), Seq(1)) // implicit
        (A.Bar(Map("bar" -> 1)): A).transformInto[C] ==> C.Bar(Seq("bar"), Seq(1)) // derived, using implicit
      }

      test("transforming a coproduct with identical structure") {
        val bagA: Bag[A] = Bag(Seq(A.Foo("foo"), A.Bar(Map("bar" -> 1))))
        bagA.transformInto[Bag[B]] ==> Bag(Seq(B.Foo("foo"), B.Bar(Map("bar" -> 1))))
      }

      test("transforming a coproduct with different structure") {
        val bagA: Bag[A] = Bag(Seq(A.Foo("foo"), A.Bar(Map("bar" -> 1))))
        bagA.transformInto[Bag[C]] ==> Bag(Seq(C.Foo("foo"), C.Bar(Seq("bar"), Seq(1))))
      }
    }

    test("with A.Bar to C transformer") {
      implicit val aBarToC: Transformer[A.Bar, C] = Issue199.barToCTransformer

      test("transforming a product") {
        A.Bar(Map("bar" -> 1)).transformInto[C] ==> C.Bar(Seq("bar"), Seq(1)) // implicit
        (A.Bar(Map("bar" -> 1)): A).transformInto[C] ==> C.Bar(Seq("bar"), Seq(1)) // derived, using implicit
      }

      test("transforming a coproduct with identical structure") {
        val bagA: Bag[A] = Bag(Seq(A.Foo("foo"), A.Bar(Map("bar" -> 1))))
        bagA.transformInto[Bag[B]] ==> Bag(Seq(B.Foo("foo"), B.Bar(Map("bar" -> 1))))
      }

      test("transforming a coproduct with different structure") {
        val bagA: Bag[A] = Bag(Seq(A.Foo("foo"), A.Bar(Map("bar" -> 1))))
        bagA.transformInto[Bag[C]] ==> Bag(Seq(C.Foo("foo"), C.Bar(Seq("bar"), Seq(1))))
      }
    }
  }

  test("fix issue #210") {
    import Issue210.*

    (B.Foo: B).transformInto[A] ==> A.Foo
    (B.Bar: B).transformInto[A] ==> A.Bar

    // make sure the other way around is fine with partial transformers
    (A.Foo: A)
      .intoPartial[B]
      .withSealedSubtypeHandledPartial[A.Unrecognized](_ => partial.Result.fromEmpty)
      .transform
      .asOption ==> Some(B.Foo)
    (A.Bar: A)
      .intoPartial[B]
      .withSealedSubtypeHandledPartial[A.Unrecognized](_ => partial.Result.fromEmpty)
      .transform
      .asOption ==> Some(B.Bar)
    (A.Unrecognized(100): A)
      .intoPartial[B]
      .withSealedSubtypeHandledPartial[A.Unrecognized](_ => partial.Result.fromEmpty)
      .transform
      .asOption ==> None
  }

  group("fix issue #209") {

    case class RawData(id: String)
    case class Data(id: Int)

    implicit val alwaysFailingPT: PartialTransformer[String, Int] =
      PartialTransformer(_ => partial.Result.fromErrorString("always fails"))

    test("without any modifiers") {
      RawData("any").transformIntoPartial[Data].asErrorPathMessageStrings ==> Iterable(
        "id" -> "always fails"
      )
    }

    test("withFieldComputedPartial") {
      val result = RawData("any")
        .intoPartial[Data]
        .withFieldComputedPartial(_.id, _.id.transformIntoPartial[Int])
        .transform

      result.asErrorPathMessageStrings ==> Iterable(
        "id" -> "always fails"
      )

      val definedPT = PartialTransformer
        .define[RawData, Data]
        .withFieldComputedPartial(_.id, _.id.transformIntoPartial[Int])
        .buildTransformer

      definedPT.transform(RawData("any")).asErrorPathMessageStrings ==> Iterable(
        "id" -> "always fails"
      )
    }

    test("withFieldConstPartial") {
      val result = RawData("any")
        .intoPartial[Data]
        .withFieldConstPartial(_.id, partial.Result.fromErrorString("always fails"))
        .transform

      result.asErrorPathMessageStrings ==> Iterable(
        "id" -> "always fails"
      )

      val definedPT = PartialTransformer
        .define[RawData, Data]
        .withFieldConstPartial(_.id, partial.Result.fromErrorString("always fails"))
        .buildTransformer

      definedPT.transform(RawData("any")).asErrorPathMessageStrings ==> Iterable(
        "id" -> "always fails"
      )
    }

    test("fix issue #297") {
      import Issue297.*

      Foo("b").transformInto[Bar] ==> Bar("b")
      Bar("b").transformInto[Foo] ==> Foo("b")
      Foo("b").into[Bar2].withFieldConst(_.number, 3).transform ==> Bar2("b", 3)
    }
  }

  test("fix issue #228") {
    import Issue228.*

    implicit val sourceToTarget: PartialTransformer[Source, Target] = PartialTransformer
      .define[Source, Target]
      .withSealedSubtypeHandledPartial[Source.Empty.type](_ => partial.Result.fromErrorString("Error"))
      .buildTransformer

    (Source.Value1(100): Source).transformIntoPartial[Target].asEither ==> Right(Target.Value1(100))

    (Source.Empty: Source)
      .transformIntoPartial[Target]
      .asEither
      .left
      .map(_.errors.iterator.map(_.message.asString).mkString) ==> Left(
      "Error"
    )
  }

  test("fix issue #291") {
    import Issue291.*

    val foo = Bar(new GenericValueClass("barToFoo")).transformInto[Foo]
    foo.address.get.value ==> "barToFoo"
  }

  test("fix issue #297") {
    import Issue297.*

    Foo("b").transformInto[Bar] ==> Bar("b")
    Bar("b").transformInto[Foo] ==> Foo("b")
    Foo("b").into[Bar2].withFieldConst(_.number, 3).transform ==> Bar2("b", 3)
  }

  test("fix issue #400") {
    import Issue400.*

    Foo("b").transformInto[Bar] ==> Bar("b")
    Foo("b").into[Bar].transform ==> Bar("b")
    Foo("b").transformIntoPartial[Bar].asOption ==> Some(Bar("b"))
    Foo("b").intoPartial[Bar].transform.asOption ==> Some(Bar("b"))
    Foo("b").into[Bar2].enableDefaultValues.transform ==> Bar2("b", "ok")
    Foo("b").intoPartial[Bar2].enableDefaultValues.transform.asOption ==> Some(Bar2("b", "ok"))
  }

  test("fix issue #403") {
    import Issue403.*

    (new Foo()).into[Bar].withFieldRenamed(_.baz1(), _.getBaz2()).transform.getBaz2() ==> "test"
  }

  test("fix issue #434") {
    import Issue434.*

    val expected = MyClass(1, 2, "asd", None)
    val actual = (1, 2, "asd").into[MyClass].enableOptionDefaultsToNone.transform
    actual ==> expected
  }

  test("fix issue #449") {
    case class Proto(isSomething: Boolean, somethingDetail: Option[Proto.SomethingDetail])
    object Proto {
      case class SomethingDetail(a: String, b: Int)
    }

    case class Domain(isSomething: Boolean, something: Option[Domain.Something])
    object Domain {
      case class Something(a: String, b: Int)
    }

    compileErrors(
      """
      Proto(isSomething = true, somethingDetail = Some(Proto.SomethingDetail("hello", 1)))
        .intoPartial[Domain]
        .withFieldRenamed(_.somethingDetail, _.something)
        .transform
      """
    ).check(
      "Chimney can't derive transformation from io.scalaland.chimney.IssuesSpec.Proto to io.scalaland.chimney.IssuesSpec.Domain",
      "io.scalaland.chimney.IssuesSpec.Domain",
      "field something: io.scalaland.chimney.IssuesSpec.Domain could not resolve overrides since the current BeanAware: TransformedNamedComparison treats the following overrides as the same: .withFieldRenamed(_.somethingDetail, _.isSomething}), .withFieldRenamed(_.somethingDetail, _.something}) making it ambiguous - change the field name comparator with .enableCustomFieldNameComparison to resolve the ambiguity",
      "Consult https://chimney.readthedocs.io for usage examples."
    )

    Proto(isSomething = true, somethingDetail = Some(Proto.SomethingDetail("hello", 1)))
      .intoPartial[Domain]
      .withFieldRenamed(_.somethingDetail, _.something)
      .enableCustomFieldNameComparison(TransformedNamesComparison.StrictEquality)
      .transform
      .asOption ==> Some(Domain(isSomething = true, something = Some(Domain.Something("hello", 1))))

    Proto(isSomething = true, somethingDetail = Some(Proto.SomethingDetail("hello", 1)))
      .intoPartial[Domain]
      .withFieldConst(_.something, None)
      .enableCustomFieldNameComparison(TransformedNamesComparison.StrictEquality)
      .transform
      .asOption ==> Some(Domain(isSomething = true, something = None))
  }

  test("fix issue #461") {

    val uuid1 = "value 1"
    val uuid2 = "value 2"

    case class From(x: String)
    case class To(uuid: String, setUuid: String, x: String)

    From("test")
      .into[To]
      .withFieldConst(_.uuid, uuid1)
      .withFieldConst(_.setUuid, uuid2)
      .enableCustomFieldNameComparison(TransformedNamesComparison.StrictEquality)
      .transform ==> To(uuid1, uuid2, "test")
  }

  test("fix issue #498") {
    import Issue498.*
    (Foo.Sub1("test"): Foo).into[Bar].withFieldConst(_.b, 1).transform ==> Bar.Sub1("test", 1)
  }
}
