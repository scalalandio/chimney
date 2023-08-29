package io.scalaland.chimney

import munit.{Location, TestOptions}

import scala.util.matching.Regex

trait ChimneySpec extends munit.BaseFunSuite with VersionCompat { self =>

  private var prefix = ""

  private def appendName(prefix: String, name: String): String = if (prefix.isEmpty) name else s"$prefix / $name"

  def group(name: String)(body: => Any): Unit = {
    val oldPrefix = prefix
    prefix = appendName(prefix, name)
    body
    prefix = oldPrefix
  }

  override def test(name: String)(body: => Any)(implicit loc: Location): Unit =
    super.test(appendName(prefix, name))(body)

  override def test(options: TestOptions)(body: => Any)(implicit loc: Location): Unit =
    if (options.name.startsWith(prefix)) super.test(options)(body)
    else super.test(options.withName(appendName(prefix, options.name)))(body)

  implicit class ArrowAssert(lhs: Any) {
    def ==>[V](rhs: V): Unit =
      (lhs, rhs) match {
        // Hack to make Arrays compare sanely; at some point we may want some
        // custom, extensible, typesafe equality check but for now this will do
        case (lhs: Array[?], rhs: Array[?]) =>
          Predef.assert(lhs.toSeq == rhs.toSeq, s"==> assertion failed: ${lhs.toSeq} != ${rhs.toSeq}")
        case (lhs, rhs) =>
          Predef.assert(lhs == rhs, s"==> assertion failed: $lhs != $rhs")
      }
  }

  implicit class CompileErrorsCheck(msg: String) {
    
    def check(msgs: String*): Unit = for (msg <- msgs)
      Predef.assert(
        ChimneySpec.AnsiControlCode.replaceAllIn(this.msg, "").contains(msg),
        "Error message did not contain expected snippet\n" +
          "Error message\n" +
          this.msg + "\n" +
          "Expected Snippet\n" +
          msg
      )

    def arePresent(): Unit = Predef.assert(msg.nonEmpty, "Expected compilation errors")
  }
}
object ChimneySpec {

  val AnsiControlCode: Regex = "\u001b\\[([0-9]+)m".r
}
