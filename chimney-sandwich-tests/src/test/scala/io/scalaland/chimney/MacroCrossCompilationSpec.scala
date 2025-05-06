package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.scala213 as s213
import io.scalaland.chimney.scala3 as s3
import scala.util.chaining.*

class MacroCrossCompilationSpec extends ChimneySpec {

  group("Scala 2.13 compiler analyzing Scala 3 code and Scala 3 compiler analyzing Scala 2.13 code") {

    test("should handle reading from and writing to monomorphic classes") {
      s213.Monomorphic
        .Foo(1, "2", 3.0, true)
        .transformInto[s3.Monomorphic.Bar] ==> s3.Monomorphic.Bar(1, "2", 3.0)
      s3.Monomorphic
        .Foo(1, "2", 3.0, true)
        .transformInto[s213.Monomorphic.Bar] ==> s213.Monomorphic.Bar(1, "2", 3.0)
      implicit val bool2int: Transformer[Boolean, Int] = b => if (b) 1 else 0
      s213.Monomorphic
        .Foo(1, "2", 3.0, true)
        .into[s3.Monomorphic.Baz]
        .withFieldRenamed(_.c, _.c0)
        .transform ==> s3.Monomorphic.Baz(1, "2", 3.0, 1)
      s3.Monomorphic
        .Foo(1, "2", 3.0, true)
        .into[s213.Monomorphic.Baz]
        .withFieldRenamed(_.c, _.c0)
        .transform ==> s213.Monomorphic.Baz(1, "2", 3.0, 1)
    }

    test("should handle reading from and writing to polymorphic classes") {
      s213.Polymorphic
        .Foo(1, "2", 3.0, true)
        .transformInto[s3.Polymorphic.Bar[Int, String]] ==> s3.Polymorphic.Bar(1, "2", 3.0)
      s3.Polymorphic
        .Foo(1, "2", 3.0, true)
        .transformInto[s213.Polymorphic.Bar[Int, String]] ==> s213.Polymorphic.Bar(1, "2", 3.0)
      implicit val bool2int: Transformer[Boolean, Int] = b => if (b) 1 else 0
      s213.Polymorphic
        .Foo(1, "2", 3.0, true)
        .into[s3.Polymorphic.Baz[Int, String]]
        .withFieldRenamed(_.c, _.c0)
        .transform ==> s3.Polymorphic.Baz(1, "2", 3.0, 1)
      s3.Polymorphic
        .Foo(1, "2", 3.0, true)
        .into[s213.Polymorphic.Baz[Int, String]]
        .withFieldRenamed(_.c, _.c0)
        .transform ==> s213.Polymorphic.Baz(1, "2", 3.0, 1)
    }

    test("should handle reading and using default values") {
      ().into[s3.Defaults.Foo].enableDefaultValues.transform ==> s3.Defaults.Foo()
      ().into[s213.Defaults.Foo].enableDefaultValues.transform ==> s213.Defaults.Foo()
    }

    test("should handle reading from and writing to case classes with @BeanProperty") {
      // TODO: there are some differences in behavior between Scala 2 and Scala 3 that we should eliminate.

      // @BeanProperties can generate both getA and A, so we can ignore getters when reading from them.
      (new s213.BeanProperties.Foo())
        .tap(_.a = 1)
        .tap(_.b = "2")
        .tap(_.c = 3.0)
        .tap(_.d = true)
        .into[s3.BeanProperties.Bar]
        .enableBeanSetters
        .transform ==> (new s3.BeanProperties.Bar())
        .tap(_.a = 1)
        .tap(_.b = "2")
        .tap(_.c = 3.0)
      (new s3.BeanProperties.Foo())
        .tap(_.a = 1)
        .tap(_.b = "2")
        .tap(_.c = 3.0)
        .tap(_.d = true)
        .into[s213.BeanProperties.Bar]
        .enableBeanSetters
        .transform ==> (new s213.BeanProperties.Bar())
        .tap(_.a = 1)
        .tap(_.b = "2")
        .tap(_.c = 3.0)
    }

    test("should handle reading from and writing to sealed trait/enum") {
      (s213.Sealed.Foo.A: s213.Sealed.Foo).transformInto[s3.Sealed.Foo] ==> s3.Sealed.Foo.A
      (s213.Sealed.Foo.B: s213.Sealed.Foo).transformInto[s3.Sealed.Foo] ==> s3.Sealed.Foo.B

      (s3.Sealed.Foo.A: s3.Sealed.Foo).transformInto[s213.Sealed.Foo] ==> s213.Sealed.Foo.A
      (s3.Sealed.Foo.B: s3.Sealed.Foo).transformInto[s213.Sealed.Foo] ==> s213.Sealed.Foo.B

      (s213.Sealed.Foo.A: s213.Sealed.Foo).transformInto[s3.Enums.Foo] ==> s3.Enums.Foo.A
      (s213.Sealed.Foo.B: s213.Sealed.Foo).transformInto[s3.Enums.Foo] ==> s3.Enums.Foo.B
    }
  }
}
