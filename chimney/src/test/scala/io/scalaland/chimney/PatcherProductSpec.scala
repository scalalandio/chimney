package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.annotation.unused

class PatcherProductSpec extends ChimneySpec {

  test("""patch an product type object with a product type patch object containing a "subset" of fields""") {
    case class Foo(a: Int, b: String, c: Double)
    case class Bar(c: Double, a: Int)

    val foo = Foo(0, "", 0.0)
    val bar = Bar(10.0, 10)

    foo.patchUsing(bar) ==> Foo(10, "", 10.0)
    foo.using(bar).patch ==> Foo(10, "", 10.0)
  }

  test(
    "patch a product with non-Option field with a product type patch object with all Option fields, applying value from Some and leaving original value in None"
  ) {
    import PatchDomain.*

    case class UserPatch(email: Option[String], phone: Option[Phone])
    val update = UserPatch(email = Some("updated@example.com"), phone = None)

    exampleUser.patchUsing(update) ==> User(10, Email("updated@example.com"), Phone(1234567890L))
    exampleUser.using(update).patch ==> User(10, Email("updated@example.com"), Phone(1234567890L))
  }

  test(
    "patch a product with non-Option field with a product type patch object with some Option fields, applying value from Some and leaving original value in None"
  ) {
    import PatchDomain.*

    case class UserPatch(email: String, phone: Option[Phone])
    val update = UserPatch(email = "updated@example.com", phone = None)

    exampleUser.patchUsing(update) ==> User(10, Email("updated@example.com"), Phone(1234567890L))
    exampleUser.using(update).patch ==> User(10, Email("updated@example.com"), Phone(1234567890L))
  }

  test("patch a product with Option field with a product type patch object with some Option fields, overriding None") {
    import PatchDomain.*

    case class UserPatch(email: String, phone: Option[Phone])
    val update = UserPatch(email = "updated@example.com", phone = None)

    exampleUserWithOptionalField.patchUsing(update) ==> UserWithOptionalField(10, Email("updated@example.com"), None)
    exampleUserWithOptionalField.using(update).patch ==> UserWithOptionalField(10, Email("updated@example.com"), None)
  }

  test(
    "patch a product with Option[A] field with a product type patch object with some Option[Option[A] fields, NOT overriding None"
  ) {
    import PatchDomain.*

    case class UserPatch(email: String, phone: Option[Option[Phone]])
    val update = UserPatch(email = "updated@example.com", phone = None)

    exampleUserWithOptionalField
      .patchUsing(update) ==> UserWithOptionalField(10, Email("updated@example.com"), Some(Phone(1234567890L)))
    exampleUserWithOptionalField.using(update).patch ==> UserWithOptionalField(
      10,
      Email("updated@example.com"),
      Some(Phone(1234567890L))
    )
  }

  group("patch a product type with a product type patch object containing field of different type") {
    case class Obj(x: String)
    case class Patch(x: Int)

    test("should fail compilation if transformation is not possible") {
      // without implicit Transformer[Int, String], it doesn't compile
      compileErrors("""Obj("").patchUsing(Patch(100))""")
        .check("not supported")
      compileErrors("""Obj("").using(Patch(100)).patch""")
        .check("not supported")
    }

    test("should adapting field's type with a Transformer") {
      implicit val intToStrTransformer: Transformer[Int, String] = _.toString
      Obj("").patchUsing(Patch(100)) ==> Obj("100")
      Obj("").using(Patch(100)).patch ==> Obj("100")
    }
  }

  group("flag .ignoreRedundantPatcherFields") {
    import PatchDomain.*

    case class PatchWithRedundantField(phone: Phone, address: String)
    // note address doesn't exist in User
    val patch1 = PatchWithRedundantField(Phone(4321L), "Unknown")

    case class PatchWithAnotherRedundantField(address: String, phone: Phone)
    // note address doesn't exist in User and it's at the beginning of the case class
    val patch2 = PatchWithAnotherRedundantField("Unknown", Phone(4321L))

    test("should be disabled by default") {

      compileErrors("exampleUser.patchUsing(patch1)").check(
        "Field named 'address' not found in target patching type io.scalaland.chimney.fixtures.PatchDomain.User!"
      )
      compileErrors("exampleUser.using(patch1).patch").check(
        "Field named 'address' not found in target patching type io.scalaland.chimney.fixtures.PatchDomain.User!"
      )

      compileErrors("exampleUser.patchUsing(patch2)").check(
        "Field named 'address' not found in target patching type io.scalaland.chimney.fixtures.PatchDomain.User!"
      )
      compileErrors("exampleUser.using(patch2).patch").check(
        "Field named 'address' not found in target patching type io.scalaland.chimney.fixtures.PatchDomain.User!"
      )
    }

    test(
      """should allow to patch an product type object with a product type patch object containing redundant fields"""
    ) {
      exampleUser
        .using(patch1)
        .ignoreRedundantPatcherFields
        .patch ==>
        exampleUser.copy(phone = patch1.phone)

      exampleUser
        .using(patch2)
        .ignoreRedundantPatcherFields
        .patch ==>
        exampleUser.copy(phone = patch2.phone)

      locally {
        implicit val cfg = PatcherConfiguration.default.ignoreRedundantPatcherFields

        exampleUser.patchUsing(patch1) ==> exampleUser.copy(phone = patch1.phone)
        exampleUser.using(patch1).patch ==> exampleUser.copy(phone = patch1.phone)

        exampleUser.patchUsing(patch2) ==> exampleUser.copy(phone = patch2.phone)
        exampleUser.using(patch2).patch ==> exampleUser.copy(phone = patch2.phone)
      }
    }
  }

  group("flag .failRedundantPatcherFields") {

    test("should disable globally enabled .ignoreRedundantPatcherFields") {
      import PatchDomain.*

      case class PatchWithRedundantField(phone: Phone, address: String)
      // note address doesn't exist in User
      @unused val patch1 = PatchWithRedundantField(Phone(4321L), "Unknown")

      case class PatchWithAnotherRedundantField(address: String, phone: Phone)
      // note address doesn't exist in User and it's at the beginning of the case class
      @unused val patch2 = PatchWithAnotherRedundantField("Unknown", Phone(4321L))

      compileErrors("exampleUser.using(patch1).failRedundantPatcherFields.patch").check(
        "Field named 'address' not found in target patching type io.scalaland.chimney.fixtures.PatchDomain.User!"
      )

      compileErrors("exampleUser.using(patch2).failRedundantPatcherFields.patch").check(
        "Field named 'address' not found in target patching type io.scalaland.chimney.fixtures.PatchDomain.User!"
      )
    }
  }

  group("flag .ignoreNoneInPatch") {

    import PatchDomain.*

    case class PhonePatch(phone: Option[Phone])
    case class IntPatch(phone: Option[Long])

    test("is disabled by default") {
      exampleUserWithOptionalField.patchUsing(PhonePatch(None)) ==> exampleUserWithOptionalField.copy(phone = None)

      exampleUserWithOptionalField.patchUsing(IntPatch(None)) ==> exampleUserWithOptionalField.copy(phone = None)
    }

    test("allow ignoring Nones in patches") {
      exampleUserWithOptionalField
        .using(PhonePatch(None))
        .ignoreNoneInPatch
        .patch ==> exampleUserWithOptionalField

      exampleUserWithOptionalField
        .using(IntPatch(None))
        .ignoreNoneInPatch
        .patch ==> exampleUserWithOptionalField

      locally {
        implicit val cfg = PatcherConfiguration.default.ignoreNoneInPatch

        exampleUserWithOptionalField.patchUsing(PhonePatch(None)) ==> exampleUserWithOptionalField
        exampleUserWithOptionalField.using(PhonePatch(None)).patch ==> exampleUserWithOptionalField

        exampleUserWithOptionalField.patchUsing(IntPatch(None)) ==> exampleUserWithOptionalField
        exampleUserWithOptionalField.using(IntPatch(None)).patch ==> exampleUserWithOptionalField
      }
    }
  }

  group("flag .clearOnNoneInPatch") {

    test("should disable globally enabled .ignoreNoneInPatch") {
      import PatchDomain.*

      case class PhonePatch(phone: Option[Phone])
      case class IntPatch(phone: Option[Long])

      @unused val cfg = PatcherConfiguration.default.ignoreNoneInPatch

      exampleUserWithOptionalField
        .using(PhonePatch(None))
        .clearOnNoneInPatch
        .patch ==> exampleUserWithOptionalField.copy(phone = None)

      exampleUserWithOptionalField
        .using(IntPatch(None))
        .clearOnNoneInPatch
        .patch ==> exampleUserWithOptionalField.copy(phone = None)
    }
  }

  test("Patcher should work in nested fields") {
    case class Foo(a: Int, b: String, c: Double)
    case class Bar(c: Double, a: Int)
    case class Nested[A](value: A)

    val foo = Nested(Foo(0, "", 0.0))
    val bar = Nested(Bar(10.0, 10))

    foo.patchUsing(bar) ==> Nested(Foo(10, "", 10.0))
    foo.using(bar).patch ==> Nested(Foo(10, "", 10.0))
  }

  test("Patcher should use implicits in nested fields") {
    case class Foo(a: Int, b: String, c: Double)
    case class Bar(c: Double, a: Int)
    case class Nested[A](value: A)

    val foo = Nested(Foo(0, "", 0.0))
    val bar = Nested(Bar(10.0, 10))

    implicit val patcher: Patcher[Foo, Bar] = new Patcher[Foo, Bar] {
      def patch(obj: Foo, patch: Bar): Foo = Foo(patch.a * 2, obj.b, patch.c * 3)
    }

    foo.patchUsing(bar) ==> Nested(Foo(20, "", 30.0))
    foo.using(bar).patch ==> Nested(Foo(20, "", 30.0))
  }
}
