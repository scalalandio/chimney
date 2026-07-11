package io.scalaland.chimney

class PartialTransformerSpec extends ChimneySpec {

  private val pt1 = PartialTransformer[String, Int](str => partial.Result.fromValue(str.toInt))
  private val pt2 = PartialTransformer.fromFunction[String, Int](_.toInt)

  private val t1: Transformer[Int, Int] = _ * 2
  private val pt3 = PartialTransformer.liftTotal[Int, Int](t1)

  case class FooStr(s1: String, s2: String)
  case class Foo(s1: Int, s2: Int)

  private val pt4 = {
    implicit val strToInt: PartialTransformer[String, Int] = pt1
    PartialTransformer.derive[FooStr, Foo]
  }

  val pt5 = {
    implicit val fooStrToFoo = pt4
    PartialTransformer.derive[List[FooStr], List[Foo]]
  }

  test("transform") {

    pt1.transform("100") ==> partial.Result.fromValue(100)
    pt1.transform("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))

    pt2.transform("100") ==> partial.Result.fromValue(100)
    pt2.transform("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))

    pt3.transform(100) ==> partial.Result.fromValue(200)

    pt4.transform(FooStr("100", "200")) ==> partial.Result.fromValue(Foo(100, 200))
    pt4.transform(FooStr("abc", "xyz")).asErrorPathMessageStrings ==> Iterable(
      ("s1", """For input string: "abc""""),
      ("s2", """For input string: "xyz"""")
    )

    pt5.transform(List(FooStr("abc", "xyz"))).asErrorPathMessageStrings ==> Iterable(
      ("(0).s1", """For input string: "abc""""),
      ("(0).s2", """For input string: "xyz"""")
    )
  }

  test("transformFailFast") {
    pt1.transformFailFast("100") ==> partial.Result.fromValue(100)
    pt1.transformFailFast("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))

    pt2.transformFailFast("100") ==> partial.Result.fromValue(100)
    pt2.transformFailFast("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))

    pt3.transformFailFast(100) ==> partial.Result.fromValue(200)

    pt4.transformFailFast(FooStr("100", "200")) ==> partial.Result.fromValue(Foo(100, 200))
    pt4.transformFailFast(FooStr("abc", "xyz")).asErrorPathMessageStrings ==> Iterable(
      ("s1", """For input string: "abc"""")
      // no second error due to fail fast mode
    )

    pt5.transformFailFast(List(FooStr("abc", "xyz"))).asErrorPathMessageStrings ==> Iterable(
      ("(0).s1", """For input string: "abc"""")
    )
  }

  test("fail fast transform with dsl") {
    import io.scalaland.chimney.dsl.*

    implicit val strToInt: PartialTransformer[String, Int] = pt1

    FooStr("abc", "xyz").intoPartial[Foo].transformFailFast.asErrorPathMessageStrings ==> Iterable(
      ("s1", """For input string: "abc"""")
      // no second error due to fail fast mode
    )
  }

  test("map") {
    val toNextInt = pt1.map(_ + 1)

    toNextInt.transform("100") ==> partial.Result.fromValue(101)
    // the mapping runs only on the successful result, errors are propagated untouched
    toNextInt.transform("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))
  }

  test("mapPartial") {
    val toPositive = pt1.mapPartial { i =>
      if (i > 0) partial.Result.fromValue(i) else partial.Result.fromErrorString("must be positive")
    }

    toPositive.transform("100") ==> partial.Result.fromValue(100)
    // the mapping itself may fail
    toPositive.transform("-5").asErrorPathMessageStrings ==> Iterable(("", "must be positive"))
    // an error from the original transformation short-circuits the mapping
    toPositive.transform("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))
  }

  test("contramap") {
    case class Raw(raw: String)

    val rawToInt = pt1.contramap[Raw](_.raw)

    rawToInt.transform(Raw("100")) ==> partial.Result.fromValue(100)
    rawToInt.transform(Raw("abc")).asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))
  }

  test("contramapPartial") {
    val trimmedToInt = pt1.contramapPartial[String] { str =>
      val trimmed = str.trim
      if (trimmed.nonEmpty) partial.Result.fromValue(trimmed) else partial.Result.fromErrorString("blank input")
    }

    trimmedToInt.transform("  100  ") ==> partial.Result.fromValue(100)
    // the preprocessing itself may fail, short-circuiting the original transformation
    trimmedToInt.transform("   ").asErrorPathMessageStrings ==> Iterable(("", "blank input"))
    // a successful preprocessing still delegates errors to the original transformation
    trimmedToInt.transform("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))
  }

  test("combinators preserve fail-fast mode") {
    // pt4 accumulates two errors in the default mode
    val derivedContramap = pt4.contramap[FooStr](identity)
    derivedContramap.transform(FooStr("abc", "xyz")).asErrorPathMessageStrings ==> Iterable(
      ("s1", """For input string: "abc""""),
      ("s2", """For input string: "xyz"""")
    )
    // ...but stops at the first error in fail-fast mode, proving the flag is threaded through
    derivedContramap.transformFailFast(FooStr("abc", "xyz")).asErrorPathMessageStrings ==> Iterable(
      ("s1", """For input string: "abc"""")
    )
  }
}
