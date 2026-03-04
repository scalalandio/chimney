package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class PatcherNamedTupleSpec extends ChimneySpec {

  group("patch case class with NamedTuple") {

    test("patch case class with NamedTuple containing a subset of fields") {
      case class Foo(a: Int, b: String, c: Double)

      val foo = Foo(0, "", 0.0)
      val patch: (c: Double, a: Int) = (c = 10.0, a = 10)

      foo.patchUsing(patch) ==> Foo(10, "", 10.0)
      foo.using(patch).patch ==> Foo(10, "", 10.0)
    }

    test("patch case class with NamedTuple containing all fields") {
      case class Foo(a: Int, b: String)

      val foo = Foo(0, "")
      val patch: (a: Int, b: String) = (a = 42, b = "updated")

      foo.patchUsing(patch) ==> Foo(42, "updated")
      foo.using(patch).patch ==> Foo(42, "updated")
    }
  }

  group("patch NamedTuple with case class") {

    test("patch NamedTuple with case class containing a subset of fields") {
      case class Patch(a: Int)

      val nt: (a: Int, b: String, c: Double) = (a = 0, b = "hello", c = 3.14)
      val patch = Patch(42)

      val expected: (a: Int, b: String, c: Double) = (a = 42, b = "hello", c = 3.14)
      nt.patchUsing(patch) ==> expected
      nt.using(patch).patch ==> expected
    }
  }

  group("patch NamedTuple with NamedTuple") {

    test("patch NamedTuple with NamedTuple containing a subset of fields") {
      val nt: (a: Int, b: String, c: Double) = (a = 0, b = "hello", c = 3.14)
      val patch: (c: Double, a: Int) = (c = 99.9, a = 42)

      val expected: (a: Int, b: String, c: Double) = (a = 42, b = "hello", c = 99.9)
      nt.patchUsing(patch) ==> expected
      nt.using(patch).patch ==> expected
    }

    test("patch NamedTuple with NamedTuple containing all fields") {
      val nt: (a: Int, b: String) = (a = 0, b = "hello")
      val patch: (a: Int, b: String) = (a = 1, b = "world")

      val expected: (a: Int, b: String) = (a = 1, b = "world")
      nt.patchUsing(patch) ==> expected
      nt.using(patch).patch ==> expected
    }
  }

  group("patch with Option fields") {

    test("patch case class with NamedTuple with Option fields: Some updates, None overrides") {
      case class Foo(a: Int, b: String, c: Option[Double])

      val foo = Foo(1, "hello", Some(3.14))
      val patch: (b: Option[String], c: Option[Double]) = (b = Some("updated"), c = None)

      foo.patchUsing(patch) ==> Foo(1, "updated", None)
      foo.using(patch).patch ==> Foo(1, "updated", None)
    }

    test("patch case class with NamedTuple using .ignoreNoneInPatch") {
      case class Foo(a: Int, b: String, c: Option[Double])

      val foo = Foo(1, "hello", Some(3.14))
      val patch: (b: Option[String], c: Option[Double]) = (b = Some("updated"), c = None)

      foo.using(patch).ignoreNoneInPatch.patch ==> Foo(1, "updated", Some(3.14))

      locally {
        implicit val cfg = PatcherConfiguration.default.ignoreNoneInPatch

        foo.patchUsing(patch) ==> Foo(1, "updated", Some(3.14))
        foo.using(patch).patch ==> Foo(1, "updated", Some(3.14))
      }
    }

    test("patch NamedTuple with NamedTuple with Option fields: Some updates, None overrides") {
      val nt: (a: Int, b: String, c: Option[Double]) = (a = 1, b = "hello", c = Some(3.14))
      val patch: (b: Option[String], c: Option[Double]) = (b = Some("updated"), c = None)

      val expected: (a: Int, b: String, c: Option[Double]) = (a = 1, b = "updated", c = None)
      nt.patchUsing(patch) ==> expected
      nt.using(patch).patch ==> expected
    }

    test("patch NamedTuple with NamedTuple using .ignoreNoneInPatch") {
      val nt: (a: Int, b: String, c: Option[Double]) = (a = 1, b = "hello", c = Some(3.14))
      val patch: (b: Option[String], c: Option[Double]) = (b = Some("updated"), c = None)

      val expected: (a: Int, b: String, c: Option[Double]) = (a = 1, b = "updated", c = Some(3.14))
      nt.using(patch).ignoreNoneInPatch.patch ==> expected
    }
  }

  group("patch with .withFieldConst / .withFieldComputed / .withFieldIgnored") {

    test(".withFieldConst provides a constant value to the patched object") {
      case class Foo(a: Int, b: String, c: Double)

      val foo = Foo(0, "hello", 3.14)
      val patch: (c: Double) = (c = 99.9)

      foo.using(patch).withFieldConst(_.a, 42).patch ==> Foo(42, "hello", 99.9)
    }

    test(".withFieldComputed computes value from patch") {
      case class Foo(a: Int, b: String, c: Double)

      val foo = Foo(0, "hello", 3.14)
      val patch: (b: String) = (b = "world")

      foo.using(patch).withFieldComputed(_.a, _ => 100).patch ==> Foo(100, "world", 3.14)
    }

    test(".withFieldIgnored ignores a patch field") {
      case class Foo(a: Int, b: String, c: Double)

      val foo = Foo(0, "hello", 3.14)
      val patch: (a: Int, b: String) = (a = 42, b = "world")

      foo.using(patch).withFieldIgnored(_.b).patch ==> Foo(42, "hello", 3.14)
    }

    test(".withFieldConst on NamedTuple target") {
      val nt: (a: Int, b: String, c: Double) = (a = 0, b = "hello", c = 3.14)
      val patch: (c: Double) = (c = 99.9)

      val expected: (a: Int, b: String, c: Double) = (a = 42, b = "hello", c = 99.9)
      nt.using(patch).withFieldConst(_.a, 42).patch ==> expected
    }
  }

  group("patch with redundant fields") {

    test("should fail compilation when patch has extra fields not in target") {
      case class Foo(a: Int, b: String)

      compileErrors(
        """Foo(1, "hello").patchUsing((a = 10, b = "world", extra = 3.14))"""
      ).check("not found in target patching type")
      compileErrors(
        """Foo(1, "hello").using((a = 10, b = "world", extra = 3.14)).patch"""
      ).check("not found in target patching type")
    }

    test("should succeed with .ignoreRedundantPatcherFields") {
      case class Foo(a: Int, b: String)

      val foo = Foo(1, "hello")
      val patch: (a: Int, b: String, extra: Double) = (a = 10, b = "world", extra = 3.14)

      foo.using(patch).ignoreRedundantPatcherFields.patch ==> Foo(10, "world")

      locally {
        implicit val cfg = PatcherConfiguration.default.ignoreRedundantPatcherFields

        foo.patchUsing(patch) ==> Foo(10, "world")
        foo.using(patch).patch ==> Foo(10, "world")
      }
    }

    test("should fail on NamedTuple target with redundant patch fields") {
      compileErrors(
        """(a = 1, b = "hello").patchUsing((a = 10, b = "world", extra = 3.14))"""
      ).check("not found in target patching type")
    }

    test("should succeed on NamedTuple target with .ignoreRedundantPatcherFields") {
      val nt: (a: Int, b: String) = (a = 1, b = "hello")
      val patch: (a: Int, b: String, extra: Double) = (a = 10, b = "world", extra = 3.14)

      val expected: (a: Int, b: String) = (a = 10, b = "world")
      nt.using(patch).ignoreRedundantPatcherFields.patch ==> expected
    }
  }

  group("patch case class with NamedTuple containing type-adapted fields") {

    test("should fail when patch field type differs without implicit Transformer") {
      case class Foo(a: String)

      compileErrors(
        """Foo("hello").patchUsing((a = 42))"""
      ).check("not supported")
      compileErrors(
        """Foo("hello").using((a = 42)).patch"""
      ).check("not supported")
    }

    test("should succeed with an implicit Transformer") {
      case class Foo(a: String, b: Int)

      implicit val intToStr: Transformer[Int, String] = _.toString

      val foo = Foo("0", 10)
      val patch: (a: Int) = (a = 42)

      foo.patchUsing(patch) ==> Foo("42", 10)
      foo.using(patch).patch ==> Foo("42", 10)
    }

    test("should adapt types between NamedTuples with implicit Transformer") {
      implicit val intToStr: Transformer[Int, String] = _.toString

      val nt: (a: String, b: Int) = (a = "0", b = 10)
      val patch: (a: Int) = (a = 42)

      val expected: (a: String, b: Int) = (a = "42", b = 10)
      nt.patchUsing(patch) ==> expected
      nt.using(patch).patch ==> expected
    }
  }
}
