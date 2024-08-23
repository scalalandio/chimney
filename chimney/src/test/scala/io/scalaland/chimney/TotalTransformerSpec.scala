package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class TotalTransformerSpec extends ChimneySpec {

  test("contramap") {
    case class Id(id: String)

    case class Length(length: Int)

    trait Prefix {
      def value: String
    }

    object Prefix {
      case object FooPrefix extends Prefix {
        override def value: String = "Foo"
      }

      case object BarPrefix extends Prefix {
        override def value: String = "Bar"
      }
    }

    val stringTransformer = new Transformer[String, Length] {
      override def transform(src: String): Length = Length(src.length)
    }

    implicit val idTransformer: Transformer[Id, Length] =
      stringTransformer.contramap(_.id)

    implicit val prefixTransformer: Transformer[Prefix, Length] =
      stringTransformer.contramap(_.value)

    val id = "id"
    Id(id).into[Length].transform ==> Length(id.length)

    val prefix: Prefix = Prefix.FooPrefix
    prefix.into[Length].transform ==> Length(prefix.value.length)
  }
}
