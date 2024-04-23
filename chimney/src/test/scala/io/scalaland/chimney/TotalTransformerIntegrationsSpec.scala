package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

import scala.annotation.unused
import scala.collection.compat.*
import scala.collection.mutable

class TotalTransformerIntegrationsSpec extends ChimneySpec {

  import TotalTransformerIntegrationsSpec.*
  import TotalTransformerStdLibTypesSpec.{Bar, Foo}

  test("transform from OptionalValue into OptionalValue") {
    Possible(Foo("a")).transformInto[Possible[Bar]] ==> Possible(Bar("a"))
    (Possible.Present(Foo("a")): Possible[Foo]).transformInto[Possible[Bar]] ==> Possible(Bar("a"))
    (Possible.Nope: Possible[Foo]).transformInto[Possible[Bar]] ==> Possible.Nope
    (Possible.Nope: Possible[String]).transformInto[Possible[String]] ==> Possible.Nope
    Possible("abc").transformInto[Possible[String]] ==> Possible.Present("abc")
    Possible(Foo("a")).transformInto[Option[Bar]] ==> Option(Bar("a"))
    Option(Foo("a")).transformInto[Possible[Bar]] ==> Possible(Bar("a"))
    compileErrorsFixed("""Possible("foobar").into[None.type].transform""").check(
      "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] to scala.None",
      "scala.None",
      "derivation from possible: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] to scala.None is not supported in Chimney!",
      "Consult https://chimney.readthedocs.io for usage examples."
    )
  }

  test("transform from non-OptionalValue into OptionalValue") {
    "abc".transformInto[Possible[String]] ==> Possible.Present("abc")
    (null: String).transformInto[Possible[String]] ==> Possible.Nope
  }

  // TODO: transform from Iterable-type to Iterable-type

  // TODO: transform between Array-type and Iterable-type

  // TODO: transform into sequential type with an override

  // TODO: transform from Map-type to Map-type

  // TODO: transform between Iterables and Maps

  // TODO transform into map type with an override

  group("flag .enableOptionDefaultsToNone") {

    case class Source(x: String)
    case class TargetWithOption(x: String, y: Possible[Int])
    case class TargetWithOptionAndDefault(x: String, y: Possible[Int] = Possible.Present(42))

    test("should be turned off by default and not allow compiling OptionalValue fields with missing source") {
      compileErrorsFixed("""Source("foo").into[TargetWithOption].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source to io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("use OptionalValue.empty for fields without source nor default value when enabled") {
      Source("foo").into[TargetWithOption].enableOptionDefaultsToNone.transform ==> TargetWithOption(
        "foo",
        Possible.Nope
      )
    }

    test(
      "use OptionalValue.empty for fields without source but with default value when enabled but default values disabled"
    ) {
      Source("foo")
        .into[TargetWithOptionAndDefault]
        .enableOptionDefaultsToNone
        .transform ==> TargetWithOptionAndDefault("foo", Possible.Nope)
    }

    test("should be ignored when default value is set and default values enabled") {
      Source("foo")
        .into[TargetWithOption]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform ==> TargetWithOption("foo", Possible.Nope)
      Source("foo")
        .into[TargetWithOptionAndDefault]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform ==> TargetWithOptionAndDefault(
        "foo",
        Possible.Present(42)
      )
    }
  }

  group("flag .disableOptionDefaultsToNone") {

    @unused case class Source(x: String)
    @unused case class TargetWithOption(x: String, y: Possible[Int])

    test("should disable globally enabled .enableOptionDefaultsToNone") {
      @unused implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

      compileErrorsFixed("""Source("foo").into[TargetWithOption].disableOptionDefaultsToNone.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source to io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }
}
object TotalTransformerIntegrationsSpec {

  import integrations.*

  sealed trait Possible[+A] extends Product with Serializable
  object Possible {
    case class Present[+A](a: A) extends Possible[A]
    case object Nope extends Possible[Nothing]

    def apply[A](value: A): Possible[A] = if (value != null) Present(value) else Nope
  }

  implicit def possibleIsOptionalValue[A]: OptionalValue[Possible[A], A] = new OptionalValue[Possible[A], A] {
    override def empty: Possible[A] = Possible.Nope
    override def of(value: A): Possible[A] = Possible(value)
    override def fold[A0](oa: Possible[A], onNone: => A0, onSome: A => A0): A0 = oa match {
      case Possible.Present(value) => onSome(value)
      case Possible.Nope           => onNone
    }
  }

  class CustomCollection[+A] private (private val impl: Vector[A]) {

    def iterator: Iterator[A] = impl.iterator

    override def equals(obj: Any): Boolean = obj match {
      case customCollection: CustomCollection[?] => impl == customCollection.impl
      case _                                     => false
    }
    override def hashCode(): Int = impl.hashCode()
  }
  object CustomCollection {

    def of[A](as: A*): CustomCollection[A] = new CustomCollection(Vector(as*))
    def from[A](vector: Vector[A]): CustomCollection[A] = new CustomCollection(vector)
  }

  implicit def customCollectionIsTotallyBuildIterable[A]: TotallyBuildIterable[CustomCollection[A], A] =
    new TotallyBuildIterable[CustomCollection[A], A] {

      def totalFactory: Factory[A, CustomCollection[A]] = new FactoryCompat[A, CustomCollection[A]] {

        override def newBuilder: mutable.Builder[A, CustomCollection[A]] =
          new FactoryCompat.Builder[A, CustomCollection[A]] {
            private val implBuilder = Vector.newBuilder[A]

            override def clear(): Unit = implBuilder.clear()

            override def result(): CustomCollection[A] = CustomCollection.from(implBuilder.result())

            override def addOne(elem: A): this.type = { implBuilder += elem; this }
          }
      }

      override def iterator(collection: CustomCollection[A]): Iterator[A] = collection.iterator
    }

  class NonEmptyCollection[+A] private (private val impl: Vector[A]) {

    def iterator: Iterator[A] = impl.iterator

    override def equals(obj: Any): Boolean = obj match {
      case nonEmptyCollection: NonEmptyCollection[?] => impl == nonEmptyCollection.impl
      case _                                         => false
    }
    override def hashCode(): Int = impl.hashCode()
  }
  object NonEmptyCollection {

    def of[A](a: A, as: A*): NonEmptyCollection[A] = new NonEmptyCollection(Vector((a +: as)*))
    def from[A](vector: Vector[A]): Option[NonEmptyCollection[A]] =
      if (vector.nonEmpty) Some(new NonEmptyCollection(vector)) else None
  }

  implicit def nonEmptyCollectionIsPartiallyBuildIterable[A]: PartiallyBuildIterable[NonEmptyCollection[A], A] =
    new PartiallyBuildIterable[NonEmptyCollection[A], A] {

      def partialFactory: Factory[A, partial.Result[NonEmptyCollection[A]]] =
        new FactoryCompat[A, partial.Result[NonEmptyCollection[A]]] {

          override def newBuilder: mutable.Builder[A, partial.Result[NonEmptyCollection[A]]] =
            new FactoryCompat.Builder[A, partial.Result[NonEmptyCollection[A]]] {
              private val implBuilder = Vector.newBuilder[A]

              override def clear(): Unit = implBuilder.clear()

              override def result(): partial.Result[NonEmptyCollection[A]] =
                partial.Result.fromOption(NonEmptyCollection.from(implBuilder.result()))

              override def addOne(elem: A): this.type = { implBuilder += elem; this }
            }
        }

      override def iterator(collection: NonEmptyCollection[A]): Iterator[A] = collection.iterator
    }

  class CustomMap[+K, +V] private (private val impl: Vector[(K, V)]) {

    def iterator: Iterator[(K, V)] = impl.iterator

    override def equals(obj: Any): Boolean = obj match {
      case customMap: CustomMap[?, ?] => impl == customMap.impl
      case _                          => false
    }
    override def hashCode(): Int = impl.hashCode()
  }
  object CustomMap {

    def of[K, V](pairs: (K, V)*): CustomMap[K, V] = new CustomMap(Vector(pairs*))
    def from[K, V](vector: Vector[(K, V)]): CustomMap[K, V] = new CustomMap(vector)
  }

  implicit def customMapIsTotallyBuildMap[K, V]: TotallyBuildMap[CustomMap[K, V], K, V] =
    new TotallyBuildMap[CustomMap[K, V], K, V] {

      def totalFactory: Factory[(K, V), CustomMap[K, V]] = new FactoryCompat[(K, V), CustomMap[K, V]] {

        override def newBuilder: mutable.Builder[(K, V), CustomMap[K, V]] =
          new FactoryCompat.Builder[(K, V), CustomMap[K, V]] {
            private val implBuilder = Vector.newBuilder[(K, V)]

            override def clear(): Unit = implBuilder.clear()

            override def result(): CustomMap[K, V] = CustomMap.from(implBuilder.result())

            override def addOne(elem: (K, V)): this.type = { implBuilder += elem; this }
          }
      }

      override def iterator(collection: CustomMap[K, V]): Iterator[(K, V)] = collection.iterator
    }

  class NonEmptyMap[+K, +V] private (private val impl: Vector[(K, V)]) {

    def iterator: Iterator[(K, V)] = impl.iterator

    override def equals(obj: Any): Boolean = obj match {
      case nonEmptyMap: NonEmptyMap[?, ?] => impl == nonEmptyMap.impl
      case _                              => false
    }
    override def hashCode(): Int = impl.hashCode()
  }
  object NonEmptyMap {

    def of[K, V](pair: (K, V), pairs: (K, V)*): NonEmptyMap[K, V] = new NonEmptyMap(Vector((pair +: pairs)*))
    def from[K, V](vector: Vector[(K, V)]): Option[NonEmptyMap[K, V]] =
      if (vector.nonEmpty) Some(new NonEmptyMap(vector)) else None
  }

  implicit def nonEmptyMapIsPartiallyBuildMap[K, V]: PartiallyBuildMap[NonEmptyMap[K, V], K, V] =
    new PartiallyBuildMap[NonEmptyMap[K, V], K, V] {

      def partialFactory: Factory[(K, V), partial.Result[NonEmptyMap[K, V]]] =
        new FactoryCompat[(K, V), partial.Result[NonEmptyMap[K, V]]] {

          override def newBuilder: mutable.Builder[(K, V), partial.Result[NonEmptyMap[K, V]]] =
            new FactoryCompat.Builder[(K, V), partial.Result[NonEmptyMap[K, V]]] {
              private val implBuilder = Vector.newBuilder[(K, V)]

              override def clear(): Unit = implBuilder.clear()

              override def result(): partial.Result[NonEmptyMap[K, V]] =
                partial.Result.fromOption(NonEmptyMap.from(implBuilder.result()))

              override def addOne(elem: (K, V)): this.type = { implBuilder += elem; this }
            }
        }

      override def iterator(collection: NonEmptyMap[K, V]): Iterator[(K, V)] = collection.iterator
    }
}
