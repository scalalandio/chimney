package io.scalaland.chimney.fixtures

import io.scalaland.chimney.Transformer

object foo {
  import io.scalaland.chimney.dsl.*

  sealed trait A extends Product with Serializable
  sealed trait AA extends A
  case object A1 extends AA

  object into {
    sealed trait A extends Product with Serializable
    sealed trait AA extends A
    case object A1 extends AA
  }

  def convert(a: A): into.A = a.transformInto[into.A]
}

case class VC(x: String) extends AnyVal

object tag {
  def apply[U] = new Tagger[U]

  trait Tagged[U]
  type @@[+T, U] = T with Tagged[U]

  class Tagger[U] {
    def apply[T](t: T): T @@ U = t.asInstanceOf[T @@ U]
  }
}

object Issue108 {
  case class Foo(i: FooA)

  sealed trait FooA extends Product with Serializable

  object FooA {
    case object A0 extends FooA
  }

  case class Bar(i: BarA)

  sealed trait BarA extends Product with Serializable

  object BarA {
    case object A0 extends BarA
  }
}

object Issue149 {
  case class EntryId(id: Int)
  case class EntryT[Id](id: Id)
  case class Patch(id: EntryId)

  case class Data[F[_]](name: F[String])
  case class Real(name: String)

  case class Data3(x: Int)
  case class Patch3[F[_]](x: F[Int])
}

object Issue156 {

  object internal {
    case class Event(venue: Venue)

    sealed trait Venue {
      def name: String
    }

    case class ManuallyFilled(name: String) extends Venue
  }

  object dto {
    case class Event(venue: Venue)
    case class Venue(name: String)
  }
}

object Issue199 {

  sealed trait A
  object A {
    case class Foo(s: String) extends A
    case class Bar(kvs: Map[String, Int]) extends A
  }

  sealed trait B
  object B {
    case class Foo(s: String) extends B
    case class Bar(kvs: Map[String, Int]) extends B
  }

  sealed trait C
  object C {
    case class Foo(s: String) extends C
    case class Bar(keys: Seq[String], values: Seq[Int]) extends C
  }

  val barToBarTransformer: Transformer[A.Bar, C.Bar] = aBar => {
    val (keys, values) = aBar.kvs.unzip
    C.Bar(keys = keys.toSeq, values = values.toSeq)
  }

  val barToCTransformer: Transformer[A.Bar, C] = aBar => {
    val (keys, values) = aBar.kvs.unzip
    C.Bar(keys = keys.toSeq, values = values.toSeq)
  }

  case class Bag[T](xs: Seq[T])
}

object Issue210 {
  sealed abstract class A(val value: Int)
  object A {
    sealed trait Recognized extends A
    case object Foo extends A(0) with A.Recognized
    case object Bar extends A(1) with A.Recognized
    final case class Unrecognized(unrecognizedValue: Int) extends A(unrecognizedValue)
  }

  sealed trait B
  object B {
    case object Foo extends B
    case object Bar extends B
  }
}

object Issue212 {
  sealed trait OneOf

  case class Something(intValue: Int) extends OneOf
  case class SomethingElse(stringValue: String) extends OneOf

  object proto {
    case class SomethingMessage(intValue: Int)
    case class SomethingElseMessage(stringValue: String)

    sealed trait OneOf
    case class Something(value: SomethingMessage) extends OneOf
    case class SomethingElse(value: SomethingElseMessage) extends OneOf
    case object Empty extends OneOf
  }
}

object Issue228 {
  sealed trait Source
  object Source {
    case class Value1(v: Int) extends Source
    case object Empty extends Source
  }

  sealed trait Target
  object Target {
    case class Value1(v: Int) extends Target
  }
}

object Issue291 {
  final class GenericValueClass[T](val value: T) extends AnyVal

  case class Bar(address: GenericValueClass[String])
  case class Foo(address: Option[GenericValueClass[String]])
}
