package io.scalaland.chimney

import utest._

object PartialResultSpec extends TestSuite {

  val tests = Tests {

    test("asOption") {
      partial.Result.fromValue(1).asOption ==> Some(1)
      partial.Result.fromEmpty.asOption ==> None
    }

    test("asEither") {
      partial.Result.fromValue(1).asEither ==> Right(1)
      partial.Result.fromEmpty.asEither ==> Left(partial.Result.Errors(partial.Error.ofEmptyValue))
    }

    test("asErrorPathMessages") {
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

    test("asErrorPathMessageStrings") {
      partial.Result.fromValue(1).asErrorPathMessageStrings ==> Iterable.empty
      partial.Result.fromEmpty.asErrorPathMessageStrings ==> Iterable("" -> "empty value")
      partial.Result.fromErrorString("test").asErrorPathMessageStrings ==> Iterable("" -> "test")
      val exception = new NoSuchElementException("test")
      partial.Result.fromErrorThrowable(exception).asErrorPathMessageStrings ==> Iterable("" -> "test")
      partial.Result.fromErrorNotDefinedAt(100).asErrorPathMessageStrings ==> Iterable("" -> "not defined at 100")
    }

    test("map only modifies successful result") {
      case class Err(msg: String) extends Throwable(msg)

      partial.Result.fromValue(10).map(_ * 2) ==> partial.Result.fromValue(20)
      partial.Result.fromEmpty[Int].map(_ * 2) ==> partial.Result.fromEmpty[Int]
      partial.Result.fromErrorString[Int]("something bad happened").map(_ * 2) ==>
        partial.Result.fromErrorString[Int]("something bad happened")
      partial.Result.fromErrorNotDefinedAt[Int](()).map(_ * 2) ==> partial.Result.fromErrorNotDefinedAt[Int](())
      partial.Result.fromErrorThrowable[Int](Err("error just happened")).map(_ * 2) ==>
        partial.Result.fromErrorThrowable[Int](Err("error just happened"))
    }

    test("flatMap preserve sequential semantics (first error interrupts)") {
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
      result3.asOption ==> None
      result3.asEither.isLeft ==> true
      result3.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")
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
      result3.asOption ==> None
      result3.asEither.isLeft ==> true
      result3.asErrorPathMessageStrings ==> Iterable("" -> """For input string: "error"""")
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
      case class Err(msg: String) extends Throwable(msg)

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
      case class Err(msg: String) extends Throwable(msg)

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
  }
}
