package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import utest._

object PatcherSpec extends TestSuite {

  val tests = Tests {

    "patch simple objects" - {

      case class Foo(a: Int, b: String, c: Double)
      case class Bar(c: Double, a: Int)

      val foo = Foo(0, "", 0.0)
      val bar = Bar(10.0, 10)

      foo.patchUsing(bar) ==>
        Foo(10, "", 10.0)
    }

    "patch objects with value classes in patch" - {

      import TestDomain._

      val update = UpdateDetails("xyz@def.com", 123123123L)

      exampleUser.patchUsing(update) ==>
        User(10, Email("xyz@def.com"), Phone(123123123L))
    }

    "patch with incomplete patches" - {

      import TestDomain._

      case class IncompletePatch(phone: Phone, address: String)
      // note address doesn't exist in User

      val patch = IncompletePatch(Phone(4321L), "Unknown")

      compileError("exampleUser.patchUsing(patch)")
        .check("", "Field named 'address' not found in target patching type io.scalaland.chimney.TestDomain.User!")

      exampleUser
        .using(patch)
        .enableIncompletePatches
        .patch ==>
        exampleUser.copy(phone = patch.phone)
    }

    "support optional types in patch" - {

      import TestDomain._

      case class UserPatch(email: Option[String], phone: Option[Phone])

      val update = UserPatch(email = Some("updated@example.com"), phone = None)

      exampleUser.patchUsing(update) ==>
        User(10, Email("updated@example.com"), Phone(1234567890L))
    }

    "support mixed optional and regular types" - {

      import TestDomain._

      case class UserPatch(email: String, phone: Option[Phone])
      val update = UserPatch(email = "updated@example.com", phone = None)

      exampleUser.patchUsing(update) ==>
        User(10, Email("updated@example.com"), Phone(1234567890L))
    }

    "optional fields in the patched object overwritten by None" - {

      import TestDomain._

      case class UserPatch(email: String, phone: Option[Phone])
      val update = UserPatch(email = "updated@example.com", phone = None)

      exampleUserWithOptionalField.patchUsing(update) ==>
        UserWithOptionalField(10, Email("updated@example.com"), None)
    }

    "fields of type Option[T] in the patched object not overwritten by None of type Option[Option[T]]" - {

      import TestDomain._

      case class UserWithOptional(id: Int, email: Email, phone: Option[Phone])

      case class UserPatch(email: String, phone: Option[Option[Phone]])
      val update = UserPatch(email = "updated@example.com", phone = None)

      exampleUserWithOptionalField.patchUsing(update) ==>
        UserWithOptionalField(10, Email("updated@example.com"), Some(Phone(1234567890L)))
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
