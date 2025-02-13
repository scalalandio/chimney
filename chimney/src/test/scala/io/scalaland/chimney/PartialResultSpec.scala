package io.scalaland.chimney

import scala.util.Try

class PartialResultSpec extends ChimneySpec {

  case class Err(msg: String) extends Throwable(msg)

  test("asOption should convert Value to Some and Errors to None") {
    partial.Result.fromValue(1).asOption ==> Some(1)
    partial.Result.fromEmpty.asOption ==> None
  }

  test("asEither should convert Value to Right and Errors to Left") {
    partial.Result.fromValue(1).asEither ==> Right(1)
    partial.Result.fromEmpty.asEither ==> Left(partial.Result.Errors(partial.Error.fromEmptyValue))
  }

  test("asErrorPathMessages should convert Value to empty and errors to non-empty Iterable") {
    partial.Result.fromValue(1).asErrorPathMessages ==> Iterable.empty
    partial.Result.fromEmpty.asErrorPathMessages ==> Iterable("" -> partial.ErrorMessage.EmptyValue)
    partial.Result.fromErrorString("test").asErrorPathMessages ==> Iterable(
      "" -> partial.ErrorMessage.StringMessage("test")
    )
    val exception = new NoSuchElementException()
    partial.Result.fromErrorThrowable(exception).asErrorPathMessages ==> Iterable(
      "" -> partial.ErrorMessage.ThrowableMessage(exception)
    )
    partial.Result.fromErrorNotDefinedAt(100).asErrorPathMessages ==> Iterable(
      "" -> partial.ErrorMessage.NotDefinedAt(100)
    )
  }

  test("asErrorPathMessageStrings should convert Value to empty and errors to non-empty Iterable") {
    partial.Result.fromValue(1).asErrorPathMessageStrings ==> Iterable.empty
    partial.Result.fromEmpty.asErrorPathMessageStrings ==> Iterable("" -> "empty value")
    partial.Result.fromErrorString("test").asErrorPathMessageStrings ==> Iterable("" -> "test")
    val exception = new NoSuchElementException("test")
    partial.Result.fromErrorThrowable(exception).asErrorPathMessageStrings ==> Iterable("" -> "test")
    partial.Result.fromErrorNotDefinedAt(100).asErrorPathMessageStrings ==> Iterable("" -> "not defined at 100")
  }

  test("asEitherErrorPathMessages should convert Value to Right and errors to Left non-empty Iterable") {
    partial.Result.fromValue(1).asEitherErrorPathMessages ==> Right(1)
    partial.Result.fromEmpty.asEitherErrorPathMessages ==> Left(Iterable("" -> partial.ErrorMessage.EmptyValue))
    partial.Result.fromErrorString("test").asEitherErrorPathMessages ==> Left(
      Iterable(
        "" -> partial.ErrorMessage.StringMessage("test")
      )
    )
    val exception = new NoSuchElementException()
    partial.Result.fromErrorThrowable(exception).asEitherErrorPathMessages ==> Left(
      Iterable(
        "" -> partial.ErrorMessage.ThrowableMessage(exception)
      )
    )
    partial.Result.fromErrorNotDefinedAt(100).asEitherErrorPathMessages ==> Left(
      Iterable(
        "" -> partial.ErrorMessage.NotDefinedAt(100)
      )
    )
  }

  test("asEitherErrorPathMessageStrings should convert Value to Right and errors to Left non-empty Iterable") {
    partial.Result.fromValue(1).asEitherErrorPathMessageStrings ==> Right(1)
    partial.Result.fromEmpty.asEitherErrorPathMessageStrings ==> Left(Iterable("" -> "empty value"))
    partial.Result.fromErrorString("test").asEitherErrorPathMessageStrings ==> Left(Iterable("" -> "test"))
    val exception = new NoSuchElementException("test")
    partial.Result.fromErrorThrowable(exception).asEitherErrorPathMessageStrings ==> Left(Iterable("" -> "test"))
    partial.Result.fromErrorNotDefinedAt(100).asEitherErrorPathMessageStrings ==> Left(
      Iterable("" -> "not defined at 100")
    )
  }

  test("fold collapses Result into requested value") {
    partial.Result.fromValue(1).fold(_.toString, _.asErrorPathMessageStrings.mkString(", ")) ==> "1"
    partial.Result.fromEmpty[Int].fold(_.toString, _.asErrorPathMessageStrings.mkString(", ")) ==> "(,empty value)"
    partial.Result
      .fromErrorString[Int]("test")
      .fold(_.toString, _.asErrorPathMessageStrings.mkString(", ")) ==> "(,test)"
    val exception = new NoSuchElementException("test")
    partial.Result
      .fromErrorThrowable[Int](exception)
      .fold(_.toString, _.asErrorPathMessageStrings.mkString(", ")) ==> "(,test)"
    partial.Result
      .fromErrorNotDefinedAt[Int](100)
      .fold(_.toString, _.asErrorPathMessageStrings.mkString(", ")) ==> "(,not defined at 100)"
  }

  test("map only modifies successful result") {
    partial.Result.fromValue(10).map(_ * 2) ==> partial.Result.fromValue(20)
    partial.Result.fromEmpty[Int].map(_ * 2) ==> partial.Result.fromEmpty[Int]
    partial.Result.fromErrorString[Int]("something bad happened").map(_ * 2) ==>
      partial.Result.fromErrorString[Int]("something bad happened")
    partial.Result.fromErrorNotDefinedAt[Int](()).map(_ * 2) ==> partial.Result.fromErrorNotDefinedAt[Int](())
    partial.Result.fromErrorThrowable[Int](Err("error just happened")).map(_ * 2) ==>
      partial.Result.fromErrorThrowable[Int](Err("error just happened"))
  }

  test("flatMap preserves sequential semantics (first error interrupts)") {
    val result = for {
      value1 <- partial.Result.fromCatching("10".toInt)
      value2 <- partial.Result.fromCatching("20".toInt)
    } yield value1 + value2
    result.asOption ==> Some(30)
    result.asEither ==> Right(30)
    result.asErrorPathMessageStrings ==> Iterable()

    val result2 = (for {
      value1 <- partial.Result.fromCatching("10".toInt)
      value2 <- partial.Result.fromCatching("error2".toInt)
    } yield value1 + value2)
    result2.asOption ==> None
    result2.asEither.isLeft ==> true
    result2.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error2"""")

    val result3 = (for {
      value1 <- partial.Result.fromCatching("error1".toInt)
      value2 <- partial.Result.fromCatching("error2".toInt)
    } yield value1 + value2)
    result3.asOption ==> None
    result3.asEither.isLeft ==> true
    result3.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error1"""")
  }

  test("flatten collapses nested Results") {
    val result = partial.Result.fromValue(partial.Result.fromValue(30)).flatten
    result.asOption ==> Some(30)
    result.asEither ==> Right(30)
    result.asErrorPathMessageStrings ==> Iterable()

    val result2 = partial.Result.fromValue(partial.Result.fromCatching("error2".toInt)).flatten
    result2.asOption ==> None
    result2.asEither.isLeft ==> true
    result2.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error2"""")
  }

  test("orElse fallbacks on another Result on error, aggregating errors if both Results fail") {
    var used = false
    val result = partial.Result.fromValue(1).orElse {
      used = true
      partial.Result.fromValue(2)
    }
    result.asOption ==> Some(1)
    result.asEither ==> Right(1)
    result.asErrorPathMessageStrings ==> Iterable()
    used ==> false

    var used2 = false
    val result2 = partial.Result.fromEmpty[Int].orElse {
      used2 = true
      partial.Result.fromValue(2)
    }
    result2.asOption ==> Some(2)
    result2.asEither ==> Right(2)
    result2.asErrorPathMessageStrings ==> Iterable()
    used2 ==> true

    var used3 = false
    val result3 = partial.Result.fromEmpty[Int].orElse {
      used3 = true
      partial.Result.fromEmpty[Int]
    }
    result3.asOption ==> None
    result3.asEither.isLeft ==> true
    result3.asErrorPathMessageStrings ==> Iterable("" -> """empty value""", "" -> """empty value""")
    used3 ==> true
  }

  test("fromFunction converts function into function returning Result") {
    partial.Result.fromFunction[Int, Int](_ * 2).apply(3) ==> partial.Result.fromValue(6)
  }

  test("fromPartialFunction converts PartialFunction into total function returning Result") {
    val f = partial.Result.fromPartialFunction[Int, Int] {
      case n if n > 0 => n * 2
    }

    f.apply(3) ==> partial.Result.fromValue(6)
    f.apply(0) ==> partial.Result.fromErrorNotDefinedAt(0)
  }

  test("fromError wraps one Error") {
    val err = partial.Error.fromEmptyValue
    partial.Result.fromError(err) ==> partial.Result.Errors(err)
  }

  test("fromErrors wraps multiple Errors") {
    val err1 = partial.Error.fromEmptyValue
    val err2 = partial.Error.fromString("foo")
    val err3 = partial.Error.fromEmptyValue
    partial.Result.fromErrors(err1, err2, err3) ==> partial.Result.Errors(err1, err2, err3)
  }

  test("fromErrorString wraps String as Error") {
    partial.Result.fromErrorString("foo") ==> partial.Result.Errors(partial.Error.fromString("foo"))
  }

  test("fromErrorStrings wraps multiple Strings as Errors") {
    partial.Result.fromErrorStrings("foo1", "foo2") ==> partial.Result.Errors(
      partial.Error.fromString("foo1"),
      partial.Error.fromString("foo2")
    )
  }

  test("fromErrorNotDefinedAt wraps value as Error for a point without value in PartialFunction") {
    partial.Result.fromErrorNotDefinedAt(100) ==> partial.Result.Errors(partial.Error.fromNotDefinedAt(100))
  }

  test("fromOption converts Option to Value or empty Error") {
    partial.Result.fromOption(Some(1)) ==> partial.Result.fromValue(1)
    partial.Result.fromOption(None) ==> partial.Result.fromEmpty
  }

  test("fromOptionOrError converts Option to Value or provided Error") {
    partial.Result.fromOptionOrError(Some(1), partial.Error.fromString("empty")) ==> partial.Result.fromValue(1)
    partial.Result.fromOptionOrError(None, partial.Error.fromString("empty")) ==> partial.Result.fromErrorString(
      "empty"
    )
  }

  test("fromOptionOrString converts Option to Value or Error message") {
    partial.Result.fromOptionOrString(Some(1), "empty") ==> partial.Result.fromValue(1)
    partial.Result.fromOptionOrString(None, "empty") ==> partial.Result.fromErrorString("empty")
  }

  test("fromOptionOrThrowable converts Option to Value or Throwable") {
    val exception = new NoSuchElementException()
    partial.Result.fromOptionOrThrowable(Some(1), exception) ==> partial.Result.fromValue(1)
    partial.Result.fromOptionOrThrowable(None, exception) ==> partial.Result.fromErrorThrowable(exception)
  }

  test("fromEither converts Either to Result") {
    partial.Result.fromEither(Right(1)) ==> partial.Result.fromValue(1)
    partial.Result.fromEither(
      Left(partial.Result.Errors.single(partial.Error.fromString("foo")))
    ) ==> partial.Result.fromErrorString("foo")
  }

  test("fromEitherString converts Either to Result wrapping Left String as Error") {
    partial.Result.fromEitherString(Right(1)) ==> partial.Result.fromValue(1)
    partial.Result.fromEitherString(Left("foo")) ==> partial.Result.fromErrorString("foo")
  }

  test("fromTry converts Try to Result wrapping Throwable as Error") {
    val exception = new NoSuchElementException()
    partial.Result.fromTry(Try(1)) ==> partial.Result.fromValue(1)
    partial.Result.fromTry(Try(throw exception)) ==> partial.Result.fromErrorThrowable(exception)
  }

  test("fromCatching converts thunk to Result caching Throwable as Error") {
    val exception = new NoSuchElementException()
    partial.Result.fromCatching(1) ==> partial.Result.fromValue(1)
    partial.Result.fromCatching(throw exception) ==> partial.Result.fromErrorThrowable(exception)
  }

  test("fromCatchingNonFatal converts thunk to Result caching NonFatal Throwable as Error") {
    val nseEx = new NoSuchElementException("oops")
    partial.Result.fromCatchingNonFatal(1) ==> partial.Result.fromValue(1)
    partial.Result.fromCatchingNonFatal(throw nseEx) ==> partial.Result.fromErrorThrowable(nseEx)
  }

  test("fromCatchingNonFatal propagates Fatal Throwable") {
    try
      partial.Result.fromCatchingNonFatal(throw new OutOfMemoryError("oops"))
    catch {
      case _: VirtualMachineError =>
        ()
      case th: Throwable =>
        throw th
    }
  }

  test(
    "traverse with failFast = false preserves parallel semantics (both branches are executed even if one of them fails)"
  ) {
    var operations = 0
    val result = partial.Result.traverse[List[Int], String, Int](
      Iterator("1", "2", "3", "4"),
      s => {
        operations += 1
        partial.Result.fromCatching(s.toInt)
      },
      failFast = false
    )
    operations ==> 4
    result.asOption ==> Some(List(1, 2, 3, 4))
    result.asEither ==> Right(List(1, 2, 3, 4))
    result.asErrorPathMessageStrings ==> Iterable()

    var operations2 = 0
    val result2 = partial.Result.traverse[List[Int], String, Int](
      Iterator("a", "b", "c", "d"),
      s => {
        operations2 += 1
        partial.Result.fromCatching(s.toInt)
      },
      failFast = false
    )
    operations2 ==> 4
    result2.asOption ==> None
    result2.asEither.isLeft ==> true
    result2.asErrorPathMessageStrings ==> Iterable(
      "" -> """For input string: "a"""",
      "" -> """For input string: "b"""",
      "" -> """For input string: "c"""",
      "" -> """For input string: "d""""
    )
  }

  test(
    "traverse with failFast = false preserves sequential semantics (first error interrupts)"
  ) {
    var operations = 0
    val result = partial.Result.traverse[List[Int], String, Int](
      Iterator("1", "2", "3", "4"),
      s => {
        operations += 1
        partial.Result.fromCatching(s.toInt)
      },
      failFast = true
    )
    operations ==> 4
    result.asOption ==> Some(List(1, 2, 3, 4))
    result.asEither ==> Right(List(1, 2, 3, 4))
    result.asErrorPathMessageStrings ==> Iterable()

    var operations2 = 0
    val result2 = partial.Result.traverse[List[Int], String, Int](
      Iterator("a", "b", "c", "d"),
      s => {
        operations2 += 1
        partial.Result.fromCatching(s.toInt)
      },
      failFast = true
    )
    operations2 ==> 1
    result2.asOption ==> None
    result2.asEither.isLeft ==> true
    result2.asErrorPathMessageStrings ==> Iterable(
      "" -> """For input string: "a""""
    )
  }

  test(
    "sequence with failFast = false preserves parallel semantics (both branches are executed even if one of them fails)"
  ) {
    var operations = 0
    val result = partial.Result.sequence[List[Int], Int](
      Iterator("1", "2", "3", "4").map { s =>
        operations += 1
        partial.Result.fromCatching(s.toInt)
      },
      failFast = false
    )
    operations ==> 4
    result.asOption ==> Some(List(1, 2, 3, 4))
    result.asEither ==> Right(List(1, 2, 3, 4))
    result.asErrorPathMessageStrings ==> Iterable()

    var operations2 = 0
    val result2 = partial.Result.sequence[List[Int], Int](
      Iterator("a", "b", "c", "d").map { s =>
        operations2 += 1
        partial.Result.fromCatching(s.toInt)
      },
      failFast = false
    )
    operations2 ==> 4
    result2.asOption ==> None
    result2.asEither.isLeft ==> true
    result2.asErrorPathMessageStrings ==> Iterable(
      "" -> """For input string: "a"""",
      "" -> """For input string: "b"""",
      "" -> """For input string: "c"""",
      "" -> """For input string: "d""""
    )
  }

  test(
    "sequence with failFast = false preserves sequential semantics (first error interrupts)"
  ) {
    var operations = 0
    val result = partial.Result.sequence[List[Int], Int](
      Iterator("1", "2", "3", "4").map { s =>
        operations += 1
        partial.Result.fromCatching(s.toInt)
      },
      failFast = true
    )
    operations ==> 4
    result.asOption ==> Some(List(1, 2, 3, 4))
    result.asEither ==> Right(List(1, 2, 3, 4))
    result.asErrorPathMessageStrings ==> Iterable()

    var operations2 = 0
    val result2 = partial.Result.sequence[List[Int], Int](
      Iterator("a", "b", "c", "d").map { s =>
        operations2 += 1
        partial.Result.fromCatching(s.toInt)
      },
      failFast = true
    )
    operations2 ==> 1
    result2.asOption ==> None
    result2.asEither.isLeft ==> true
    result2.asErrorPathMessageStrings ==> Iterable(
      "" -> """For input string: "a""""
    )
  }

  test("asErrorPathMessageStrings should convert PathElements and Errors to Strings") {

    val result = partial.Result.sequence[List[Int], Int](
      Iterator(
        partial.Result.fromValue(10),
        partial.Result.fromEmpty[Int],
        partial.Result.fromErrorString[Int]("something bad happened"),
        partial.Result.fromErrorNotDefinedAt[Int](0),
        partial.Result.fromErrorThrowable[Int](Err("error just happened"))
      ),
      failFast = false
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "" -> "empty value",
      "" -> "something bad happened",
      "" -> "not defined at 0",
      "" -> "error just happened"
    )
  }

  test("asErrorPathMessages should convert PathElements to Strings") {
    val result = partial.Result.sequence[List[Int], Int](
      Iterator(
        partial.Result.fromValue(10),
        partial.Result.fromEmpty[Int],
        partial.Result.fromErrorString[Int]("something bad happened"),
        partial.Result.fromErrorNotDefinedAt[Int](()),
        partial.Result.fromErrorThrowable[Int](Err("error just happened"))
      ),
      failFast = false
    )
    result.asErrorPathMessages ==> Iterable(
      "" -> partial.ErrorMessage.EmptyValue,
      "" -> partial.ErrorMessage.StringMessage("something bad happened"),
      "" -> partial.ErrorMessage.NotDefinedAt(()),
      "" -> partial.ErrorMessage.ThrowableMessage(Err("error just happened"))
    )
  }

  test(
    "map2 with failFast = false preserves parallel semantics (both branches are executed even if one of them fails)"
  ) {
    var operations = 0
    val result = partial.Result.map2(
      partial.Result.fromCatching {
        operations += 1
        "10".toInt
      },
      partial.Result.fromCatching {
        operations += 1
        "20".toInt
      },
      (a: Int, b: Int) => a + b,
      failFast = false
    )
    operations ==> 2
    result.asOption ==> Some(30)
    result.asEither ==> Right(30)
    result.asErrorPathMessageStrings ==> Iterable()

    var operations2 = 0
    val result2 = partial.Result.map2(
      partial.Result.fromCatching {
        operations2 += 1
        "error".toInt
      },
      partial.Result.fromCatching {
        operations2 += 1
        "20".toInt
      },
      (a: Int, b: Int) => a + b,
      failFast = false
    )
    operations2 ==> 2
    result2.asOption ==> None
    result2.asEither.isLeft ==> true
    result2.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")

    var operations3 = 0
    val result3 = partial.Result.map2(
      partial.Result.fromCatching {
        operations3 += 1
        "10".toInt
      },
      partial.Result.fromCatching {
        operations3 += 1
        "error".toInt
      },
      (a: Int, b: Int) => a + b,
      failFast = false
    )
    operations3 ==> 2
    result3.asOption ==> None
    result3.asEither.isLeft ==> true
    result3.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")

    var operations4 = 0
    val result4 = partial.Result.map2(
      partial.Result.fromCatching {
        operations4 += 1
        "error1".toInt
      },
      partial.Result.fromCatching {
        operations4 += 1
        "error2".toInt
      },
      (a: Int, b: Int) => a + b,
      failFast = false
    )
    operations4 ==> 2
    result4.asOption ==> None
    result4.asEither.isLeft ==> true
    result4.asErrorPathMessageStrings ==> Iterable(
      "" -> """For input string: "error1"""",
      "" -> """For input string: "error2""""
    )
  }

  test("map2 with failFast = true preserves sequential semantics (first error interrupts)") {
    var operations = 0
    val result = partial.Result.map2(
      partial.Result.fromCatching {
        operations += 1
        "10".toInt
      },
      partial.Result.fromCatching {
        operations += 1
        "20".toInt
      },
      (a: Int, b: Int) => a + b,
      failFast = true
    )
    operations ==> 2
    result.asOption ==> Some(30)
    result.asEither ==> Right(30)
    result.asErrorPathMessageStrings ==> Iterable()

    var operations2 = 0
    val result2 = partial.Result.map2(
      partial.Result.fromCatching {
        operations2 += 1
        "error".toInt
      },
      partial.Result.fromCatching {
        operations2 += 1
        "20".toInt
      },
      (a: Int, b: Int) => a + b,
      failFast = true
    )
    operations2 ==> 1
    result2.asOption ==> None
    result2.asEither.isLeft ==> true
    result2.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")

    var operations3 = 0
    val result3 = partial.Result.map2(
      partial.Result.fromCatching {
        operations3 += 1
        "10".toInt
      },
      partial.Result.fromCatching {
        operations3 += 1
        "error".toInt
      },
      (a: Int, b: Int) => a + b,
      failFast = true
    )
    operations3 ==> 2
    result3.asOption ==> None
    result3.asEither.isLeft ==> true
    result3.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")

    var operations4 = 0
    val result4 = partial.Result.map2(
      partial.Result.fromCatching {
        operations4 += 1
        "error".toInt
      },
      partial.Result.fromCatching {
        operations += 1
        "error".toInt
      },
      (a: Int, b: Int) => a + b,
      failFast = true
    )
    operations4 ==> 1
    result4.asOption ==> None
    result4.asEither.isLeft ==> true
    result4.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")
  }

  test(
    "product with failFast = false preserves parallel semantics (both branches are executed even if one of them fails)"
  ) {
    var operations = 0
    val result = partial.Result.product(
      partial.Result.fromCatching {
        operations += 1
        "10".toInt
      },
      partial.Result.fromCatching {
        operations += 1
        "20".toInt
      },
      failFast = false
    )
    operations ==> 2
    result.asOption ==> Some(10 -> 20)
    result.asEither ==> Right(10 -> 20)
    result.asErrorPathMessageStrings ==> Iterable()

    var operations2 = 0
    val result2 = partial.Result.product(
      partial.Result.fromCatching {
        operations2 += 1
        "error".toInt
      },
      partial.Result.fromCatching {
        operations2 += 1
        "20".toInt
      },
      failFast = false
    )
    operations2 ==> 2
    result2.asOption ==> None
    result2.asEither.isLeft ==> true
    result2.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")

    var operations3 = 0
    val result3 = partial.Result.product(
      partial.Result.fromCatching {
        operations3 += 1
        "10".toInt
      },
      partial.Result.fromCatching {
        operations3 += 1
        "error".toInt
      },
      failFast = false
    )
    operations3 ==> 2
    result3.asOption ==> None
    result3.asEither.isLeft ==> true
    result3.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")

    var operations4 = 0
    val result4 = partial.Result.product(
      partial.Result.fromCatching {
        operations4 += 1
        "error1".toInt
      },
      partial.Result.fromCatching {
        operations4 += 1
        "error2".toInt
      },
      failFast = false
    )
    operations4 ==> 2
    result4.asOption ==> None
    result4.asEither.isLeft ==> true
    result4.asErrorPathMessageStrings ==> Iterable(
      "" -> """For input string: "error1"""",
      "" -> """For input string: "error2""""
    )
  }

  test("product with failFast = true preserves sequential semantics (first error interrupts)") {
    var operations = 0
    val result = partial.Result.product(
      partial.Result.fromCatching {
        operations += 1
        "10".toInt
      },
      partial.Result.fromCatching {
        operations += 1
        "20".toInt
      },
      failFast = true
    )
    operations ==> 2
    result.asOption ==> Some(10 -> 20)
    result.asEither ==> Right(10 -> 20)
    result.asErrorPathMessageStrings ==> Iterable()

    var operations2 = 0
    val result2 = partial.Result.product(
      partial.Result.fromCatching {
        operations2 += 1
        "error".toInt
      },
      partial.Result.fromCatching {
        operations2 += 1
        "20".toInt
      },
      failFast = true
    )
    operations2 ==> 1
    result2.asOption ==> None
    result2.asEither.isLeft ==> true
    result2.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")

    var operations3 = 0
    val result3 = partial.Result.product(
      partial.Result.fromCatching {
        operations3 += 1
        "10".toInt
      },
      partial.Result.fromCatching {
        operations3 += 1
        "error".toInt
      },
      failFast = true
    )
    operations3 ==> 2
    result3.asOption ==> None
    result3.asEither.isLeft ==> true
    result3.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")

    var operations4 = 0
    val result4 = partial.Result.product(
      partial.Result.fromCatching {
        operations4 += 1
        "error".toInt
      },
      partial.Result.fromCatching {
        operations += 1
        "error".toInt
      },
      failFast = true
    )
    operations4 ==> 1
    result4.asOption ==> None
    result4.asEither.isLeft ==> true
    result4.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")
  }

  group("import partial.syntax.* should") {

    import io.scalaland.chimney.partial.syntax.*

    test("allow lifting Option to partial.Result with extension methods") {
      (Some(1): Option[Int]).asResult.asEitherErrorPathMessageStrings ==> Right(1)
      (None: Option[Int]).asResult.asEitherErrorPathMessageStrings ==> Left(Iterable("" -> "empty value"))

      (Some(1): Option[Int])
        .orErrorAsResult(partial.Error.fromString("It was empty"))
        .asEitherErrorPathMessageStrings ==> Right(1)
      (None: Option[Int])
        .orErrorAsResult(partial.Error.fromString("It was empty"))
        .asEitherErrorPathMessageStrings ==> Left(Iterable("" -> "It was empty"))

      (Some(1): Option[Int])
        .orStringAsResult("It was empty!")
        .asEitherErrorPathMessageStrings ==> Right(1)
      (None: Option[Int])
        .orStringAsResult("It was empty!")
        .asEitherErrorPathMessageStrings ==> Left(Iterable("" -> "It was empty!"))

      (Some(1): Option[Int])
        .orThrowableAsResult(new Throwable("It was empty!!"))
        .asEitherErrorPathMessageStrings ==> Right(1)
      (None: Option[Int])
        .orThrowableAsResult(new Throwable("It was empty!!"))
        .asEitherErrorPathMessageStrings ==> Left(Iterable("" -> "It was empty!!"))
    }

    test("allow lifting Either to partial.Result with extension method") {
      (Right(1): Either[partial.Result.Errors, Int]).asResult.asEitherErrorPathMessageStrings ==> Right(1)
      (Left(partial.Result.Errors.fromString("some error")): Either[
        partial.Result.Errors,
        Int
      ]).asResult.asEitherErrorPathMessageStrings ==> Left(Iterable("" -> "some error"))

      (Right(1): Either[String, Int]).asResult.asEitherErrorPathMessageStrings ==> Right(1)
      (Left("some error"): Either[String, Int]).asResult.asEitherErrorPathMessageStrings ==> Left(
        Iterable("" -> "some error")
      )
    }

    test("allow lifting Try to partial.Result with extension methods") {
      Try(1).asResult.asEitherErrorPathMessageStrings ==> Right(1)
      Try("error".toInt).asResult.asEitherErrorPathMessageStrings ==> Left(
        Iterable("" -> """For input string: "error"""")
      )
    }
  }
}
