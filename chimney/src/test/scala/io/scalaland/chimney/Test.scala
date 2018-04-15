package io.scalaland.chimney

import scala.language.experimental.macros

case class Source(zzz: Int)
case class Target(zzz: Int)

case class Foo(x: Int, z: Boolean, y: String, abc: Source, u: Int, zzz: RuntimeException, kukuku: Unit)
case class Bar1(y: String, x: Int, abc: Target, u: String, zzz: Exception)
case class Bar2(y: String, lululu: Unit, x: Int, abc: Target, u: String)



object Test extends App {

  implicit val intToStringTransformer: Transformer[Int, String] = (_: Int).toString

  val t = Transformer.gen[Foo, Bar1]

  val u = t.transform(Foo(10, false, "abc", Source(9999), 0, new RuntimeException,  ()))

  //val yyy = Test.this.intToStringTransformer.transform(4)

  println(u)

  val t2 = Transformer.gen[Foo, Bar2]
}
