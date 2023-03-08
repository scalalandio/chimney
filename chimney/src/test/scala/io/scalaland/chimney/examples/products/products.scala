package io.scalaland.chimney.examples.products

import io.scalaland.chimney.*

object Domain1 {

  case class UserName(value: String)

  val userNameToStringTransformer: Transformer[UserName, String] =
    (userName: UserName) => userName.value + "T"
  val userNameToStringPartialTransformer: PartialTransformer[UserName, String] =
    (userName: UserName, _) => partial.Result.fromValue(userName.value + "T")

  case class UserDTO(id: String, name: String)
  case class User(id: String, name: UserName)
}

object Poly {

  case class MonoSource(poly: String, other: String)
  case class PolySource[T](poly: T, other: String)
  case class MonoTarget(poly: String, other: String)
  case class PolyTarget[T](poly: T, other: String)

  val monoSource = MonoSource("test", "test")
  val polySource = PolySource("test", "test")
  val monoTarget = MonoTarget("test", "test")
  val polyTarget = PolyTarget("test", "test")
}

object NonCaseDomain {

  class ClassSource(val id: String, val name: String)

  trait TraitSource {
    val id: String
    val name: String
  }

  class TraitSourceImpl(val id: String, val name: String) extends TraitSource
}

case class Foo(x: Int, y: String, z: (Double, Double))
case class Bar(x: Int, z: (Double, Double))
case class HaveY(y: String)

object Renames {

  case class User(id: Int, name: String, age: Option[Int])
  case class UserPL(id: Int, imie: String, wiek: Either[Unit, Int])
  case class UserPLStd(id: Int, imie: String, wiek: Option[Int])
  case class User2ID(id: Int, name: String, age: Option[Int], extraID: Int)
  case class UserPLStrict(id: Int, imie: String, wiek: Int)

  def ageToWiekTransformer: Transformer[Option[Int], Either[Unit, Int]] =
    new Transformer[Option[Int], Either[Unit, Int]] {
      def transform(obj: Option[Int]): Either[Unit, Int] =
        obj.fold[Either[Unit, Int]](Left(()))(Right.apply)
    }
}

object Defaults {

  case class Source(xx: Int, yy: String, z: Double)
  case class Target(x: Int = 10, y: String = "y", z: Double)
  case class Target2(xx: Long = 10, yy: String = "y", z: Double)

  case class Nested[A](value: A)
}
