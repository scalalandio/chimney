package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import utest._

object PatcherSpec extends TestSuite {

  val tests = Tests {

    test("patch simple objects") {

      case class Foo(a: Int, b: String, c: Double)
      case class Bar(c: Double, a: Int)

      val foo = Foo(0, "", 0.0)
      val bar = Bar(10.0, 10)

      foo.patchUsing(bar) ==>
        Foo(10, "", 10.0)
    }

    test("patch objects with value classes in patch") {

      import TestDomain._

      val update = UpdateDetails("xyz@def.com", 123123123L)

      exampleUser.patchUsing(update) ==>
        User(10, Email("xyz@def.com"), Phone(123123123L))
    }

    test("patch with redundant fields") {

      import TestDomain._

      case class PatchWithRedundantField(phone: Phone, address: String)
      // note address doesn't exist in User

      val patch = PatchWithRedundantField(Phone(4321L), "Unknown")

      compileError("exampleUser.patchUsing(patch)")
        .check(
          "",
          "Field named 'address' not found in target patching type io.scalaland.chimney.TestDomain.User!"
        )

      exampleUser
        .using(patch)
        .ignoreRedundantPatcherFields
        .patch ==>
        exampleUser.copy(phone = patch.phone)
    }

    test("patch with redundant fields at the beginning") {

      import TestDomain._

      case class PatchWithAnotherRedundantField(address: String, phone: Phone)
      // note address doesn't exist in User and it's at the beginning of the case class

      val patch = PatchWithAnotherRedundantField("Unknown", Phone(4321L))

      compileError("exampleUser.patchUsing(patch)")
        .check(
          "",
          "Field named 'address' not found in target patching type io.scalaland.chimney.TestDomain.User!"
        )

      exampleUser
        .using(patch)
        .ignoreRedundantPatcherFields
        .patch ==>
        exampleUser.copy(phone = patch.phone)
    }

    test("support optional types in patch") {

      import TestDomain._

      case class UserPatch(email: Option[String], phone: Option[Phone])

      val update = UserPatch(email = Some("updated@example.com"), phone = None)

      exampleUser.patchUsing(update) ==>
        User(10, Email("updated@example.com"), Phone(1234567890L))
    }

    test("support mixed optional and regular types") {

      import TestDomain._

      case class UserPatch(email: String, phone: Option[Phone])
      val update = UserPatch(email = "updated@example.com", phone = None)

      exampleUser.patchUsing(update) ==>
        User(10, Email("updated@example.com"), Phone(1234567890L))
    }

    test("optional fields in the patched object overwritten by None") {

      import TestDomain._

      case class UserPatch(email: String, phone: Option[Phone])
      val update = UserPatch(email = "updated@example.com", phone = None)

      exampleUserWithOptionalField.patchUsing(update) ==>
        UserWithOptionalField(10, Email("updated@example.com"), None)
    }

    test("fields of type Option[T] in the patched object not overwritten by None of type Option[Option[T]]") {

      import TestDomain._

      case class UserWithOptional(id: Int, email: Email, phone: Option[Phone])

      case class UserPatch(email: String, phone: Option[Option[Phone]])
      val update = UserPatch(email = "updated@example.com", phone = None)

      exampleUserWithOptionalField.patchUsing(update) ==>
        UserWithOptionalField(10, Email("updated@example.com"), Some(Phone(1234567890L)))
    }

    test("allow ignoring nones in patches") {

      import TestDomain._

      case class Foo(x: Option[Int])
      case class PhonePatch(phone: Option[Phone])
      case class IntPatch(phone: Option[Long])

      exampleUserWithOptionalField.patchUsing(PhonePatch(None)) ==>
        exampleUserWithOptionalField.copy(phone = None)

      exampleUserWithOptionalField.patchUsing(IntPatch(None)) ==>
        exampleUserWithOptionalField.copy(phone = None)

      exampleUserWithOptionalField
        .using(PhonePatch(None))
        .ignoreNoneInPatch
        .patch ==> exampleUserWithOptionalField

      exampleUserWithOptionalField
        .using(IntPatch(None))
        .ignoreNoneInPatch
        .patch ==> exampleUserWithOptionalField
    }

    test("patcher with underlying transformation") {
      case class Obj(x: String)
      case class Patch(x: Int)

      test("successful") {
        implicit val intToStrTransformer: Transformer[Int, String] = _.toString
        Obj("").patchUsing(Patch(100)) ==> Obj("100")
      }

      test("failed") {
        // without implicit Transformer[Int, String], it doesn't compile
        compileError("""Obj("").patchUsing(Patch(100))""")
          .check("", "not supported")
      }
    }
  }

}

object TestDomain {

  case class Email(address: String) extends AnyVal
  case class Phone(number: Long) extends AnyVal

  case class User(id: Int, email: Email, phone: Phone)
  case class UpdateDetails(email: String, phone: Long)

  case class UserWithOptionalField(id: Int, email: Email, phone: Option[Phone])

  val exampleUser = User(10, Email("abc@def.com"), Phone(1234567890L))
  val exampleUserWithOptionalField = UserWithOptionalField(10, Email("abc@def.com"), Option(Phone(1234567890L)))

}
