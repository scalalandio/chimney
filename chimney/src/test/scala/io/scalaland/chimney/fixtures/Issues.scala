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
  def apply[Tag] = new Tagger[Tag]

  trait Tagged[Tag]
  type @@[+A, Tag] = A & Tagged[Tag]

  class Tagger[Tag] {
    def apply[A](a: A): A @@ Tag = a.asInstanceOf[A @@ Tag]
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

  case class Bag[A0](xs: Seq[A0])
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
  final class GenericValueClass[A](val value: A) extends AnyVal

  case class Bar(address: GenericValueClass[String])
  case class Foo(address: Option[GenericValueClass[String]])
}

object Issue297 {
  case class Foo(value: String) extends AnyVal
  case class Bar(value: String)
  case class Bar2(value: String, number: Int)
}

object Issue400 {
  case class Foo(value: String)
  case class Bar(var value: String)
  case class Bar2(var value: String, var value2: String = "ok")
}

object Issue403 {
  class Foo {
    def baz1(): String = "test"
  }

  class Bar {
    private var baz = ""
    def getBaz2(): String = baz
    def setBaz2(baz: String): Unit = this.baz = baz
  }
}

object Issue434 {
  case class MyClass(x: Int, y: Int, z: String, zz: Option[Int])
}

object Issue498 {
  sealed trait Foo
  object Foo {
    case class Sub1(a: String) extends Foo
    case class Sub2(a: String) extends Foo
  }

  sealed trait Bar {
    def b: Int
  }
  object Bar {
    case class Sub1(a: String, b: Int) extends Bar
    case class Sub2(a: String, b: Int) extends Bar
  }
}

object Issue579 {
  case class Foo(bar: Option[Bar])
  case class Bar(baz: List[Baz])
  case class Baz(a: Int, b: String, c: Double)
}

object Issue479 {
  sealed trait color
  object color {
    case object orange extends color
    case object pink extends color
    case object yellow extends color
  }

  sealed trait Target
  object Target {
    case class Impl(value: String) extends Target
  }
}
