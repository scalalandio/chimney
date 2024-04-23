package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class TotalTransformerScala3StdLibTypesSpec extends ChimneySpec {

  import TotalTransformerStdLibTypesSpec.*

  test("transform from Array-type to Array-type") {
    IArray(Foo("a")).transformInto[IArray[Bar]] ==> IArray(Bar("a"))
    IArray("a").transformInto[IArray[String]] ==> IArray("a")
  }

  test("transform between Array-type and Iterable-type") {
    IArray(Foo("a")).transformInto[List[Bar]] ==> List(Bar("a"))
    IArray("a", "b").transformInto[Seq[String]] ==> Seq("a", "b")
    IArray(3, 2, 1).transformInto[Vector[Int]] ==> Vector(3, 2, 1)

    Vector("a").transformInto[IArray[String]] ==> IArray("a")
    List(1, 6, 3).transformInto[IArray[Int]] ==> IArray(1, 6, 3)
    Seq(Bar("x"), Bar("y")).transformInto[IArray[Foo]] ==> IArray(Foo("x"), Foo("y"))
  }

  // almost working :/
  /*
  test("transform into sequential type with an override") {
    Array[Foo](Foo("a"))
      .into[IArray[Bar]]
      .enableMacrosLogging
      .withFieldConst(_.everyItem.value, "override")
      .transform
      .unsafeArray ==> Array(Bar("override"))
    IArray[Foo](Foo("a"))
      .into[Array[Bar]]
      .withFieldConst(_.everyItem.value, "override")
      .transform ==> Array(Bar("override"))
  }
   */
}
