package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class TotalTransformerSpec extends ChimneySpec {

  // SAM syntax still works after adding the combinators (single abstract method preserved)
  private val stringLength: Transformer[String, Int] = _.length

  test("map") {
    case class Length(length: Int)

    val toLength: Transformer[String, Length] = stringLength.map(Length(_))

    toLength.transform("test") ==> Length(4)

    // the produced instance is usable as an implicit for derivation
    implicit val t: Transformer[String, Length] = toLength
    "test".into[Length].transform ==> Length(4)
  }

  test("contramap") {
    case class Id(id: String)

    val idLength: Transformer[Id, Int] = stringLength.contramap(_.id)

    idLength.transform(Id("test")) ==> 4

    implicit val t: Transformer[Id, Int] = idLength
    Id("test").into[Int].transform ==> 4
  }

  test("map and contramap compose") {
    case class Id(id: String)
    case class Length(length: Int)

    val idToLength: Transformer[Id, Length] = stringLength.contramap[Id](_.id).map(Length(_))

    idToLength.transform(Id("hello")) ==> Length(5)
  }
}
