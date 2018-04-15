//package io.scalaland.chimney
//
//import org.scalatest.{MustMatchers, WordSpec}
//import io.scalaland.chimney.dsl._
//
//class PatcherSpec extends WordSpec with MustMatchers {
//
//  "Patcher DSL" should {
//
//    "patch simple objects" in {
//
//      case class Foo(a: Int, b: String, c: Double)
//      case class Bar(c: Double, a: Int)
//
//      val foo = Foo(0, "", 0.0)
//      val bar = Bar(10.0, 10)
//
//      foo.patchWith(bar) mustBe
//        Foo(10, "", 10.0)
//    }
//
//    "patch objects with value classes in patch" in {
//
//      import TestDomain._
//
//      val update = UpdateDetails("xyz@def.com", 123123123L)
//
//      exampleUser.patchWith(update) mustBe
//        User(10, Email("xyz@def.com"), Phone(123123123L))
//    }
//
//    "support optional types in patch" in {
//
//      import TestDomain._
//
//      case class UserPatch(email: Option[String], phone: Option[Phone])
//
//      val update = UserPatch(email = Some("updated@example.com"), phone = None)
//
//      exampleUser.patchWith(update) mustBe
//        User(10, Email("updated@example.com"), Phone(1234567890L))
//    }
//
//    "support mixed optional and regular types" in {
//
//      import TestDomain._
//
//      case class UserPatch(email: String, phone: Option[Phone])
//      val update = UserPatch(email = "updated@example.com", phone = None)
//
//      exampleUser.patchWith(update) mustBe User(10, Email("updated@example.com"), Phone(1234567890L))
//    }
//  }
//
//}
//
//object TestDomain {
//
//  case class Email(address: String) extends AnyVal
//  case class Phone(number: Long) extends AnyVal
//
//  case class User(id: Int, email: Email, phone: Phone)
//  case class UpdateDetails(email: String, phone: Long)
//
//  val exampleUser = User(10, Email("abc@def.com"), Phone(1234567890L))
//
//}
