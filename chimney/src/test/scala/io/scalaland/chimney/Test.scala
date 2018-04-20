package io.scalaland.chimney

import scala.language.experimental.macros

case class Source(zzz: Int)
case class Target(zzz: Int)

case class Foo(x: Int,
               z: Boolean,
               y: String,
               abc: Source,
               u: Int,
               zzz: RuntimeException,
               kukuku: Unit,
               lululu: Char,
               kaka: Float)

case class Bar1(y: String, x: Int, abc: Target, u: String, zzz: Exception)
case class Bar2(y: String, lululu: Unit, x: Int, abc: Target, u: String, kaka: String)
case class Bar3(koo: Double = 3.0, u: Int)
case class Bar4(abc: String)

import io.scalaland.chimney.dsl._

object Test extends App {


//  val instance = {
//    val __chimney$computed_abc: io.scalaland.chimney.Foo => String = ((x$3: io.scalaland.chimney.Foo) => x$3.toString());
//    {
//      new _root_.io.scalaland.chimney.Transformer[io.scalaland.chimney.Foo, io.scalaland.chimney.Bar4] {
//        def transform(foo$3: io.scalaland.chimney.Foo): io.scalaland.chimney.Bar4 = new io.scalaland.chimney.Bar4(__chimney$computed_abc(foo$3))
//      }
//    }
//  }

  implicit val intToStringTransformer: Transformer[Int, String] = (_: Int).toString

  val foo = Foo(10, false, "abc", Source(9999), 0, new RuntimeException, (), 'x', 2.0f)

  println(foo.transformInto[Bar1])

  println(foo.into[Bar4].withFieldConst(_.abc, "abc").transform)
  println(foo.into[Bar4].withFieldComputed(_.abc, _.toString).transform)


//  println(foo.into[Bar1].disableDefaultValues.transform)
//  println(foo.into[Bar1].withFieldConst(_.u, "dupa").transform)
  println {
    foo
      .into[Bar1]
      .withFieldConst(_.zzz, new RuntimeException("lalala"))
      .disableDefaultValues
      .withFieldConst(_.u, "dupa")
      .withFieldComputed(_.y, _.u.toString)
      .withFieldConst(_.zzz, new Exception)
      .transform
  }

//  println(foo.transformInto[Bar3])
}
