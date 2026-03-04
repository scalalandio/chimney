package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class TotalTransformerNamedTupleSpec extends ChimneySpec {
  group("transform between case classes and NamedTuples") {
    case class Foo(a: Int, b: String, c: Double)

    test("case class to NamedTuple with matching field names") {
      val expected = (a = 1, b = "test", c = 3.14)
      Foo(1, "test", 3.14).transformInto[(a: Int, b: String, c: Double)] ==> expected
    }

    test("NamedTuple to case class with matching field names") {
      val nt: (a: Int, b: String, c: Double) = (a = 1, b = "test", c = 3.14)
      nt.transformInto[Foo] ==> Foo(1, "test", 3.14)
    }

    test("case class to NamedTuple with field name mismatch should fail") {
      compileErrors("""Foo(1, "test", 3.14).transformInto[(a: Int, b: String, z: Double)]""").check(
        s"""|Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerNamedTupleSpec.Foo to scala.NamedTuple.NamedTuple[scala.Tuple3["a", "b", "z"], scala.Tuple3[scala.Int, java.lang.String, scala.Double]]
            |
            |scala.NamedTuple.NamedTuple[scala.Tuple3["a", "b", "z"], scala.Tuple3[scala.Int, java.lang.String, scala.Double]]
            |  z: scala.Double - no accessor named z in source type io.scalaland.chimney.TotalTransformerNamedTupleSpec.Foo
            |
            |Consult https://chimney.readthedocs.io for usage examples.
            |""".stripMargin
      )
    }

    test("case class to NamedTuple with field name mismatch should success with .withFieldRenamed") {
      val expected = (a = 1, b = "test", z = 3.14)
      Foo(1, "test", 3.14).into[(a: Int, b: String, z: Double)].withFieldRenamed(_.c, _.z).transform ==> expected
    }

    test("NamedTuple to case class with field name mismatch should fail") {
      compileErrors("""(a = 1, b = "test", z = 3.14).transformInto[Foo]""").check(
        s"""|Chimney can't derive transformation from scala.NamedTuple.NamedTuple[scala.Tuple3["a", "b", "z"], scala.Tuple3[scala.Int, java.lang.String, scala.Double]] to io.scalaland.chimney.TotalTransformerNamedTupleSpec.Foo
            |
            |io.scalaland.chimney.TotalTransformerNamedTupleSpec.Foo
            |  c: scala.Double - no accessor named c in source type scala.NamedTuple.NamedTuple[scala.Tuple3["a", "b", "z"], scala.Tuple3[scala.Int, java.lang.String, scala.Double]]
            |
            |Consult https://chimney.readthedocs.io for usage examples.
            |""".stripMargin
      )
    }

    test("NamedTuple to case class with field name mismatch should success with .withFieldRenamed") {
      val expected = Foo(1, "test", 3.14)
      (a = 1, b = "test", z = 3.14).into[Foo].withFieldRenamed(_.z, _.c).transform ==> expected
    }

    test("case class with more fields to NamedTuple with fewer fields") {
      val expected = (a = 1, b = "test")
      Foo(1, "test", 3.14).transformInto[(a: Int, b: String)] ==> expected
    }

    test("NamedTuple with fewer fields to case class should fail") {
      compileErrors("""(a = 1, b = "test").transformInto[Foo]""").check(
        s"""|Chimney can't derive transformation from scala.NamedTuple.NamedTuple[scala.Tuple2["a", "b"], scala.Tuple2[scala.Int, java.lang.String]] to io.scalaland.chimney.TotalTransformerNamedTupleSpec.Foo
            |
            |io.scalaland.chimney.TotalTransformerNamedTupleSpec.Foo
            |  c: scala.Double - no accessor named c in source type scala.NamedTuple.NamedTuple[scala.Tuple2["a", "b"], scala.Tuple2[scala.Int, java.lang.String]]
            |
            |Consult https://chimney.readthedocs.io for usage examples.
            |""".stripMargin
      )
    }

    test("Recursive transformation between case classes and NamedTuples") {
      case class Bar(foo: Foo, flag: Boolean)

      val expected = (foo = (a = 1, b = "x", c = 2.0), flag = true)
      Bar(Foo(1, "x", 2.0), true).transformInto[(foo: (a: Int, b: String, c: Double), flag: Boolean)] ==> expected

      val nt: (foo: (a: Int, b: String, c: Double), flag: Boolean) = expected
      nt.transformInto[Bar] ==> Bar(Foo(1, "x", 2.0), true)
    }
  }

  group("transform NamedTuples to NamedTuples") {

    test("NamedTuple to NamedTuple with matching names") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val expected = (a = 1, b = "hello")
      nt.transformInto[(a: Int, b: String)] ==> expected
    }

    test("NamedTuple to NamedTuple with matching names and more source fields") {
      val nt: (a: Int, b: String, c: Double) = (a = 1, b = "hello", c = 3.14)
      val expected = (a = 1, b = "hello")
      nt.transformInto[(a: Int, b: String)] ==> expected
    }

    test("NamedTuple to NamedTuple with mismatched names should fail") {
      compileErrors("""(a = 1, b = "hello").transformInto[(x: Int, y: String)]""").check(
        s"""|Chimney can't derive transformation from scala.NamedTuple.NamedTuple[scala.Tuple2["a", "b"], scala.Tuple2[scala.Int, java.lang.String]] to scala.NamedTuple.NamedTuple[scala.Tuple2["x", "y"], scala.Tuple2[scala.Int, java.lang.String]]
            |
            |scala.NamedTuple.NamedTuple[scala.Tuple2["x", "y"], scala.Tuple2[scala.Int, java.lang.String]]
            |  x: scala.Int - no accessor named x in source type scala.NamedTuple.NamedTuple[scala.Tuple2["a", "b"], scala.Tuple2[scala.Int, java.lang.String]]
            |  y: java.lang.String - no accessor named y in source type scala.NamedTuple.NamedTuple[scala.Tuple2["a", "b"], scala.Tuple2[scala.Int, java.lang.String]]
            |
            |Consult https://chimney.readthedocs.io for usage examples.
            |""".stripMargin
      )
    }
  }

  group("transform between NamedTuples and regular Tuples") {

    test("NamedTuple to regular Tuple") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val expected = (1, "hello")
      nt.transformInto[(Int, String)] ==> expected
    }

    test("regular Tuple to NamedTuple") {
      val expected = (a = 1, b = "hello")
      (1, "hello").transformInto[(a: Int, b: String)] ==> expected
    }

    test("NamedTuple to regular Tuple with mismatched types should fail") {
      compileErrors("""(a = 1, b = "hello").transformInto[(String, Int)]""").check(
        s"""|Chimney can't derive transformation from scala.NamedTuple.NamedTuple[scala.Tuple2["a", "b"], scala.Tuple2[scala.Int, java.lang.String]] to scala.Tuple2[java.lang.String, scala.Int]
            |
            |scala.Tuple2[java.lang.String, scala.Int]
            |  _1: java.lang.String - can't derive transformation from _1: scala.Int in source type scala.NamedTuple.NamedTuple[scala.Tuple2["a", "b"], scala.Tuple2[scala.Int, java.lang.String]]
            |  _2: scala.Int - can't derive transformation from _2: java.lang.String in source type scala.NamedTuple.NamedTuple[scala.Tuple2["a", "b"], scala.Tuple2[scala.Int, java.lang.String]]
            |
            |java.lang.String (transforming from: a into: _1)
            |  derivation from namedtuple.asInstanceOf[scala.Tuple2[scala.Int, java.lang.String]]._1: scala.Int to java.lang.String is not supported in Chimney!
            |
            |scala.Int (transforming from: b into: _2)
            |  derivation from namedtuple.asInstanceOf[scala.Tuple2[scala.Int, java.lang.String]]._2: java.lang.String to scala.Int is not supported in Chimney!
            |
            |Consult https://chimney.readthedocs.io for usage examples.
            |""".stripMargin
      )
    }
  }

  group("transform NamedTuples with .withField* DSL") {
    test("transform NamedTuple with .withFieldConst") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val expected = (a = 2, b = "hello")
      nt.into[(a: Int, b: String)].withFieldConst(_.a, 2).transform ==> expected
    }

    test("transform NamedTuple with .withFieldRenamed") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val expected = (a = 1, z = "hello")
      nt.into[(a: Int, z: String)].withFieldRenamed(_.b, _.z).transform ==> expected
    }

    test("transform NamedTuple with .withFieldComputed") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val expected = (a = 1, z = "hello world")
      nt.into[(a: Int, z: String)].withFieldComputed(_.z, _.b + " world").transform ==> expected
    }
  }

  group("transform NamedTuples with Lens-like DSL") {
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
      val cc = Foo(
        Some(
          Bar(
            List(
              Baz(1, "hello", 3.14),
              Baz(2, "world", 4.14)
            )
          )
        )
      )
      val nt: NT = (bar = Some((baz = Nil)))
      cc.into[NT].withFieldConst(_.bar.matchingSome.baz, Nil).transform ==> nt
    }

    test("transform case class to NamedTuple with .matchingSome and .everyItem") {
      val cc = Foo(
        Some(Bar(List(Baz(1, "hello", 3.14), Baz(2, "world", 4.14))))
      )
      val expected: NT = (bar = Some((baz = List((a = 10, b = "new", c = 3.14), (a = 10, b = "new", c = 4.14)))))
      cc.into[NT]
        .withFieldConst(_.bar.matchingSome.baz.everyItem.a, 10)
        .withFieldConst(_.bar.matchingSome.baz.everyItem.b, "new")
        .transform ==> expected
    }

    test("transform NamedTuple to case class with .matchingSome") {
      val nt: NT = (bar = Some((baz = List((a = 1, b = "hello", c = 3.14), (a = 2, b = "world", c = 4.14)))))
      nt.into[Foo]
        .withFieldConst(_.bar.matchingSome.baz, Nil)
        .transform ==> Foo(Some(Bar(Nil)))
    }

    test("transform NamedTuple to NamedTuple with .matchingSome and .everyItem") {
      val nt: NT = (bar = Some((baz = List((a = 1, b = "hello", c = 3.14), (a = 2, b = "world", c = 4.14)))))
      val expected: NT = (bar = Some((baz = List((a = 99, b = "hello", c = 3.14), (a = 99, b = "world", c = 4.14)))))
      nt.into[NT]
        .withFieldConst(_.bar.matchingSome.baz.everyItem.a, 99)
        .transform ==> expected
    }

    test("transform with .matchingLeft and .matchingRight on Either NamedTuple fields") {
      val expectedLeft: EitherNT = (value = Left((a = 10, b = "hello", c = 3.14)))
      EitherCC(Left(Baz(1, "hello", 3.14)))
        .into[EitherNT]
        .withFieldConst(_.value.matchingLeft.a, 10)
        .withFieldConst(_.value.matchingRight.a, 20)
        .transform ==> expectedLeft

      val expectedRight: EitherNT = (value = Right((a = 20, b = "world", c = 4.14)))
      EitherCC(Right(Baz(2, "world", 4.14)))
        .into[EitherNT]
        .withFieldConst(_.value.matchingLeft.a, 10)
        .withFieldConst(_.value.matchingRight.a, 20)
        .transform ==> expectedRight
    }

    test("transform with .everyMapValue on Map NamedTuple fields") {
      val mapValue: (a: Int, b: String, c: Double) = (a = 10, b = "hello", c = 3.14)
      val expected: MapNT = (data = Map("x" -> mapValue))
      MapCC(Map("x" -> Baz(1, "hello", 3.14)))
        .into[MapNT]
        .withFieldConst(_.data.everyMapValue.a, 10)
        .transform ==> expected
    }

    test("transform with deep nesting combining .matchingSome, .everyMapValue, and .everyItem") {
      val cc = DeepOuter(Some(DeepMiddle(Map("key" -> DeepInner(List(Baz(1, "hello", 3.14)))))))
      val innerValue: (values: List[(a: Int, b: String, c: Double)]) =
        (values = List((a = 99, b = "hello", c = 3.14)))
      val expected: DeepNT = (opt = Some((items = Map("key" -> innerValue))))
      cc.into[DeepNT]
        .withFieldConst(_.opt.matchingSome.items.everyMapValue.values.everyItem.a, 99)
        .transform ==> expected
    }

    test("transform NamedTuple to NamedTuple with .withFieldComputed using lens path") {
      val nt: NT = (bar = Some((baz = List((a = 1, b = "hello", c = 3.14), (a = 2, b = "world", c = 4.14)))))
      val expected: NT = (bar = Some((baz = List((a = 1, b = "hello", c = 3.14)))))
      nt.into[NT]
        .withFieldComputed(_.bar.matchingSome.baz, _.bar.map(_.baz.take(1)).getOrElse(Nil))
        .transform ==> expected
    }
  }

  group("NamedTuple boundary arities") {
    test("NamedTuple.Empty - Empty NamedTuple") {
      import TotalTransformerNamedTupleSpec.*

      val expected = Fields0()
      Fields0().transformInto[NamedTuple.Empty] ==> NamedTuple.Empty

      val nt: NamedTuple.Empty = NamedTuple.Empty
      nt.transformInto[Fields0] ==> expected
    }

    test("Tuple1 - single element NamedTuple") {
      import TotalTransformerNamedTupleSpec.*

      val expected = (f1 = 42)
      Fields1(42).transformInto[NT1] ==> expected

      val nt: NT1 = (f1 = 42)
      nt.transformInto[Fields1] ==> Fields1(42)
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

      cc.transformInto[NT23] ==> nt
      nt.transformInto[Fields23] ==> cc
    }
  }
}
object TotalTransformerNamedTupleSpec {
  case class Fields0()
  case class Fields1(f1: Int)
  case class Fields23(
      f1: Int,
      f2: Int,
      f3: Int,
      f4: Int,
      f5: Int,
      f6: Int,
      f7: Int,
      f8: Int,
      f9: Int,
      f10: Int,
      f11: Int,
      f12: Int,
      f13: Int,
      f14: Int,
      f15: Int,
      f16: Int,
      f17: Int,
      f18: Int,
      f19: Int,
      f20: Int,
      f21: Int,
      f22: Int,
      f23: Int
  )

  type NT1 = (f1: Int)
  type NT23 = (
      f1: Int,
      f2: Int,
      f3: Int,
      f4: Int,
      f5: Int,
      f6: Int,
      f7: Int,
      f8: Int,
      f9: Int,
      f10: Int,
      f11: Int,
      f12: Int,
      f13: Int,
      f14: Int,
      f15: Int,
      f16: Int,
      f17: Int,
      f18: Int,
      f19: Int,
      f20: Int,
      f21: Int,
      f22: Int,
      f23: Int
  )
}
