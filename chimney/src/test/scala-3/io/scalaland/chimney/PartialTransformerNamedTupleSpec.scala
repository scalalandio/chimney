package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.partial.syntax.*

class PartialTransformerNamedTupleSpec extends ChimneySpec {

  group("partially transform between case classes and NamedTuples") {
    case class Foo(a: Int, b: String, c: Double)

    test("case class to NamedTuple with matching field names") {
      val expected = (a = 1, b = "test", c = 3.14)
      Foo(1, "test", 3.14).transformIntoPartial[(a: Int, b: String, c: Double)].asOption ==> Some(expected)
    }

    test("NamedTuple to case class with matching field names") {
      val nt: (a: Int, b: String, c: Double) = (a = 1, b = "test", c = 3.14)
      nt.transformIntoPartial[Foo].asOption ==> Some(Foo(1, "test", 3.14))
    }

    test("case class to NamedTuple with field name mismatch should fail") {
      compileErrors(
        """Foo(1, "test", 3.14).transformIntoPartial[(a: Int, b: String, z: Double)]"""
      ).check(
        "no accessor named z in source type"
      )
    }

    test("case class to NamedTuple with field name mismatch should succeed with .withFieldRenamed") {
      val expected = (a = 1, b = "test", z = 3.14)
      Foo(1, "test", 3.14)
        .intoPartial[(a: Int, b: String, z: Double)]
        .withFieldRenamed(_.c, _.z)
        .transform
        .asOption ==> Some(expected)
    }

    test("NamedTuple to case class with field name mismatch should fail") {
      compileErrors(
        """(a = 1, b = "test", z = 3.14).transformIntoPartial[Foo]"""
      ).check(
        "no accessor named c in source type"
      )
    }

    test("NamedTuple to case class with field name mismatch should succeed with .withFieldRenamed") {
      (a = 1, b = "test", z = 3.14)
        .intoPartial[Foo]
        .withFieldRenamed(_.z, _.c)
        .transform
        .asOption ==> Some(Foo(1, "test", 3.14))
    }

    test("case class with more fields to NamedTuple with fewer fields") {
      val expected = (a = 1, b = "test")
      Foo(1, "test", 3.14).transformIntoPartial[(a: Int, b: String)].asOption ==> Some(expected)
    }

    test("NamedTuple with fewer fields to case class should fail") {
      compileErrors(
        """(a = 1, b = "test").transformIntoPartial[Foo]"""
      ).check(
        "no accessor named c in source type"
      )
    }

    test("Recursive transformation between case classes and NamedTuples") {
      case class Bar(foo: Foo, flag: Boolean)

      val expected = (foo = (a = 1, b = "x", c = 2.0), flag = true)
      Bar(Foo(1, "x", 2.0), true)
        .transformIntoPartial[(foo: (a: Int, b: String, c: Double), flag: Boolean)]
        .asOption ==> Some(expected)

      val nt: (foo: (a: Int, b: String, c: Double), flag: Boolean) = expected
      nt.transformIntoPartial[Bar].asOption ==> Some(Bar(Foo(1, "x", 2.0), true))
    }
  }

  group("partially transform NamedTuples to NamedTuples") {

    test("NamedTuple to NamedTuple with matching names") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val expected = (a = 1, b = "hello")
      nt.transformIntoPartial[(a: Int, b: String)].asOption ==> Some(expected)
    }

    test("NamedTuple to NamedTuple with matching names and more source fields") {
      val nt: (a: Int, b: String, c: Double) = (a = 1, b = "hello", c = 3.14)
      val expected = (a = 1, b = "hello")
      nt.transformIntoPartial[(a: Int, b: String)].asOption ==> Some(expected)
    }

    test("NamedTuple to NamedTuple with mismatched names should fail") {
      compileErrors(
        """(a = 1, b = "hello").transformIntoPartial[(x: Int, y: String)]"""
      ).check(
        "no accessor named x in source type",
        "no accessor named y in source type"
      )
    }
  }

  group("partially transform between NamedTuples and regular Tuples") {

    test("NamedTuple to regular Tuple") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      nt.transformIntoPartial[(Int, String)].asOption ==> Some((1, "hello"))
    }

    test("regular Tuple to NamedTuple") {
      val expected = (a = 1, b = "hello")
      (1, "hello").transformIntoPartial[(a: Int, b: String)].asOption ==> Some(expected)
    }

    test("NamedTuple to regular Tuple with mismatched types should fail") {
      compileErrors(
        """(a = 1, b = "hello").transformIntoPartial[(String, Int)]"""
      ).check(
        "is not supported in Chimney"
      )
    }
  }

  group("partially transform NamedTuples with .withField* DSL") {

    test("transform NamedTuple with .withFieldConst") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val expected = (a = 2, b = "hello")
      nt.intoPartial[(a: Int, b: String)].withFieldConst(_.a, 2).transform.asOption ==> Some(expected)
    }

    test("transform NamedTuple with .withFieldRenamed") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val expected = (a = 1, z = "hello")
      nt.intoPartial[(a: Int, z: String)].withFieldRenamed(_.b, _.z).transform.asOption ==> Some(expected)
    }

    test("transform NamedTuple with .withFieldComputed") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val expected = (a = 1, z = "hello world")
      nt.intoPartial[(a: Int, z: String)]
        .withFieldComputed(_.z, _.b + " world")
        .transform
        .asOption ==> Some(expected)
    }

    test("transform NamedTuple with .withFieldConstPartial (success)") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val expected = (a = 42, b = "hello")
      nt.intoPartial[(a: Int, b: String)]
        .withFieldConstPartial(_.a, partial.Result.fromValue(42))
        .transform
        .asOption ==> Some(expected)
    }

    test("transform NamedTuple with .withFieldConstPartial (failure)") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      nt.intoPartial[(a: Int, b: String)]
        .withFieldConstPartial(_.a, partial.Result.fromEmpty)
        .transform
        .asOption ==> None
    }

    test("transform NamedTuple with .withFieldComputedPartial (success)") {
      val nt: (a: Int, b: String) = (a = 1, b = "42")
      val expected = (a = 1, c = 42)
      nt.intoPartial[(a: Int, c: Int)]
        .withFieldComputedPartial(_.c, src => partial.Result.fromValue(src.b.toInt))
        .transform
        .asOption ==> Some(expected)
    }

    test("transform NamedTuple with .withFieldComputedPartial (failure)") {
      val nt: (a: Int, b: String) = (a = 1, b = "not-a-number")
      nt.intoPartial[(a: Int, c: Int)]
        .withFieldComputedPartial(_.c, _ => partial.Result.fromErrorString("invalid int"))
        .transform
        .asOption ==> None
    }
  }

  group("partially transform with Option unwrapping") {

    test("NamedTuple with Option field to case class with non-Option field (Some succeeds)") {
      case class Target(a: Int, b: String)

      val nt: (a: Option[Int], b: String) = (a = Some(42), b = "hello")
      nt.transformIntoPartial[Target].asOption ==> Some(Target(42, "hello"))
    }

    test("NamedTuple with Option field to case class with non-Option field (None fails)") {
      case class Target(a: Int, b: String)

      val nt: (a: Option[Int], b: String) = (a = None, b = "hello")
      nt.transformIntoPartial[Target].asOption ==> None
    }

    test("NamedTuple with Option field to NamedTuple without Option (Some succeeds)") {
      val nt: (a: Option[Int], b: String) = (a = Some(42), b = "hello")
      val expected = (a = 42, b = "hello")
      nt.transformIntoPartial[(a: Int, b: String)].asOption ==> Some(expected)
    }

    test("NamedTuple with Option field to NamedTuple without Option (None fails)") {
      val nt: (a: Option[Int], b: String) = (a = None, b = "hello")
      nt.transformIntoPartial[(a: Int, b: String)].asOption ==> None
    }

    test("case class to NamedTuple with Option wrapping (auto-wraps)") {
      case class Source(a: Int, b: String)

      val expected: (a: Option[Int], b: String) = (a = Some(10), b = "hello")
      Source(10, "hello").transformIntoPartial[(a: Option[Int], b: String)].asOption ==> Some(expected)
    }
  }

  group("partially transform NamedTuples with Lens-like DSL") {
    case class Foo(bar: Option[Bar])
    case class Bar(baz: List[Baz])
    case class Baz(a: Int, b: String, c: Double)
    case class EitherCC(value: Either[Baz, Baz])
    case class MapCC(data: Map[String, Baz])
    case class DeepOuter(opt: Option[DeepMiddle])
    case class DeepMiddle(items: Map[String, DeepInner])
    case class DeepInner(values: List[Baz])

    type NT = (bar: Option[(baz: List[(a: Int, b: String, c: Double)])])
    type EitherNT = (value: Either[(a: Int, b: String, c: Double), (a: Int, b: String, c: Double)])
    type MapNT = (data: Map[String, (a: Int, b: String, c: Double)])
    type DeepNT = (opt: Option[(items: Map[String, (values: List[(a: Int, b: String, c: Double)])])])

    test("transform case class to NamedTuple with .matchingSome") {
      val cc = Foo(Some(Bar(List(Baz(1, "hello", 3.14), Baz(2, "world", 4.14)))))
      val nt: NT = (bar = Some((baz = Nil)))
      cc.intoPartial[NT]
        .withFieldConst(_.bar.matchingSome.baz, Nil)
        .transform
        .asOption ==> Some(nt)
    }

    test("transform case class to NamedTuple with .matchingSome and .everyItem") {
      val cc = Foo(Some(Bar(List(Baz(1, "hello", 3.14), Baz(2, "world", 4.14)))))
      val expected: NT = (bar = Some((baz = List((a = 10, b = "new", c = 3.14), (a = 10, b = "new", c = 4.14)))))
      cc.intoPartial[NT]
        .withFieldConst(_.bar.matchingSome.baz.everyItem.a, 10)
        .withFieldConst(_.bar.matchingSome.baz.everyItem.b, "new")
        .transform
        .asOption ==> Some(expected)
    }

    test("transform NamedTuple to case class with .matchingSome") {
      val nt: NT = (bar = Some((baz = List((a = 1, b = "hello", c = 3.14), (a = 2, b = "world", c = 4.14)))))
      nt.intoPartial[Foo]
        .withFieldConst(_.bar.matchingSome.baz, Nil)
        .transform
        .asOption ==> Some(Foo(Some(Bar(Nil))))
    }

    test("transform NamedTuple to NamedTuple with .matchingSome and .everyItem") {
      val nt: NT = (bar = Some((baz = List((a = 1, b = "hello", c = 3.14), (a = 2, b = "world", c = 4.14)))))
      val expected: NT = (bar = Some((baz = List((a = 99, b = "hello", c = 3.14), (a = 99, b = "world", c = 4.14)))))
      nt.intoPartial[NT]
        .withFieldConst(_.bar.matchingSome.baz.everyItem.a, 99)
        .transform
        .asOption ==> Some(expected)
    }

    test("transform with .matchingLeft and .matchingRight on Either NamedTuple fields") {
      val expectedLeft: EitherNT = (value = Left((a = 10, b = "hello", c = 3.14)))
      EitherCC(Left(Baz(1, "hello", 3.14)))
        .intoPartial[EitherNT]
        .withFieldConst(_.value.matchingLeft.a, 10)
        .withFieldConst(_.value.matchingRight.a, 20)
        .transform
        .asOption ==> Some(expectedLeft)

      val expectedRight: EitherNT = (value = Right((a = 20, b = "world", c = 4.14)))
      EitherCC(Right(Baz(2, "world", 4.14)))
        .intoPartial[EitherNT]
        .withFieldConst(_.value.matchingLeft.a, 10)
        .withFieldConst(_.value.matchingRight.a, 20)
        .transform
        .asOption ==> Some(expectedRight)
    }

    test("transform with .everyMapValue on Map NamedTuple fields") {
      val mapValue: (a: Int, b: String, c: Double) = (a = 10, b = "hello", c = 3.14)
      val expected: MapNT = (data = Map("x" -> mapValue))
      MapCC(Map("x" -> Baz(1, "hello", 3.14)))
        .intoPartial[MapNT]
        .withFieldConst(_.data.everyMapValue.a, 10)
        .transform
        .asOption ==> Some(expected)
    }

    test("transform with deep nesting combining .matchingSome, .everyMapValue, and .everyItem") {
      val cc = DeepOuter(Some(DeepMiddle(Map("key" -> DeepInner(List(Baz(1, "hello", 3.14)))))))
      val innerValue: (values: List[(a: Int, b: String, c: Double)]) =
        (values = List((a = 99, b = "hello", c = 3.14)))
      val expected: DeepNT = (opt = Some((items = Map("key" -> innerValue))))
      cc.intoPartial[DeepNT]
        .withFieldConst(_.opt.matchingSome.items.everyMapValue.values.everyItem.a, 99)
        .transform
        .asOption ==> Some(expected)
    }

    test("transform NamedTuple to NamedTuple with .withFieldComputedPartial using lens path") {
      val nt: NT = (bar = Some((baz = List((a = 1, b = "hello", c = 3.14), (a = 2, b = "world", c = 4.14)))))
      val expected: NT = (bar = Some((baz = List((a = 1, b = "hello", c = 3.14)))))
      nt.intoPartial[NT]
        .withFieldComputedPartial(
          _.bar.matchingSome.baz,
          src => partial.Result.fromValue(src.bar.map(_.baz.take(1)).getOrElse(Nil))
        )
        .transform
        .asOption ==> Some(expected)
    }
  }

  group("NamedTuple boundary arities (partial)") {
    test("NamedTuple.Empty - Empty NamedTuple") {
      import TotalTransformerNamedTupleSpec.*

      Fields0().transformIntoPartial[NamedTuple.Empty].asOption ==> Some(NamedTuple.Empty)

      val nt: NamedTuple.Empty = NamedTuple.Empty
      nt.transformIntoPartial[Fields0].asOption ==> Some(Fields0())
    }

    test("Tuple1 - single element NamedTuple") {
      import TotalTransformerNamedTupleSpec.*

      val expected = (f1 = 42)
      Fields1(42).transformIntoPartial[NT1].asOption ==> Some(expected)

      val nt: NT1 = (f1 = 42)
      nt.transformIntoPartial[Fields1].asOption ==> Some(Fields1(42))
    }

    test("Tuple23 - NamedTuple implemented as TupleXXL") {
      import TotalTransformerNamedTupleSpec.*

      val cc = Fields23(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
      val nt: NT23 = (
        f1 = 1,
        f2 = 2,
        f3 = 3,
        f4 = 4,
        f5 = 5,
        f6 = 6,
        f7 = 7,
        f8 = 8,
        f9 = 9,
        f10 = 10,
        f11 = 11,
        f12 = 12,
        f13 = 13,
        f14 = 14,
        f15 = 15,
        f16 = 16,
        f17 = 17,
        f18 = 18,
        f19 = 19,
        f20 = 20,
        f21 = 21,
        f22 = 22,
        f23 = 23
      )

      cc.transformIntoPartial[NT23].asOption ==> Some(nt)
      nt.transformIntoPartial[Fields23].asOption ==> Some(cc)
    }
  }
}
