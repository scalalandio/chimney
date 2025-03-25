package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class TotalTransformerSpec extends ChimneySpec {

  test("map") {
    case class Length(length: Int)

    trait Prefix {
      def code: Int
    }

    object Prefix {

      def from(i: Int): Prefix = i match {
        case 1 => FooPrefix
        case 2 => BarPrefix
        case _ => NanPrefix
      }

      case object NanPrefix extends Prefix {
        override def code: Int = 0
      }

      case object FooPrefix extends Prefix {
        override def code: Int = 1
      }

      case object BarPrefix extends Prefix {
        override def code: Int = 2
      }
    }

    val stringTransformer = new Transformer[String, Int] {
      override def transform(src: String): Int = src.length
    }

    implicit val toLengthTransformer: Transformer[String, Length] =
      stringTransformer.map(Length.apply)

    implicit val toPrefixTransformer: Transformer[String, Prefix] =
      stringTransformer.map(Prefix.from)

    val id = "1"
    id.into[Length].transform ==> Length(id.length)
    id.into[Prefix].transform ==> Prefix.FooPrefix
  }

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
