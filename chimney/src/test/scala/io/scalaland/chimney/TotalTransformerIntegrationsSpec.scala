package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

import scala.annotation.unused
import scala.collection.Factory
import scala.collection.immutable.SortedSet
import scala.collection.mutable

class TotalTransformerIntegrationsSpec extends ChimneySpec {

  import TotalTransformerIntegrationsSpec.*
  import TotalTransformerStdLibTypesSpec.{Bar, Foo}

  test("transform using TotalOuterTransformer") {
    import OuterTransformers.totalNonEmptyToSorted

    implicit val barOrdering: Ordering[Bar] = Ordering[String].on[Bar](_.value)

    NonEmptyWrapper("b", "a").transformInto[SortedWrapper[String]] ==> SortedWrapper("a", "b")
    NonEmptyWrapper(Foo("b"), Foo("a")).transformInto[SortedWrapper[Bar]] ==> SortedWrapper(Bar("a"), Bar("b"))
  }

  test("transform using TotalOuterTransformer with an override") {
    import OuterTransformers.totalNonEmptyToSorted

    implicit val barOrdering: Ordering[Bar] = Ordering[String].on[Bar](_.value)

    NonEmptyWrapper("b", "a")
      .into[SortedWrapper[String]]
      .withFieldConst(_.everyItem, "c")
      .transform ==> SortedWrapper("c")
    NonEmptyWrapper(Foo("b"), Foo("a"))
      .into[SortedWrapper[Bar]]
      .withFieldConst(_.everyItem.value, "c")
      .transform ==> SortedWrapper(Bar("c"))
  }

  test("transform from OptionalValue into OptionalValue") {
    Possible(Foo("a")).transformInto[Possible[Bar]] ==> Possible(Bar("a"))
    (Possible.Present(Foo("a")): Possible[Foo]).transformInto[Possible[Bar]] ==> Possible(Bar("a"))
    (Possible.Nope: Possible[Foo]).transformInto[Possible[Bar]] ==> Possible.Nope
    (Possible.Nope: Possible[String]).transformInto[Possible[String]] ==> Possible.Nope
    Possible("abc").transformInto[Possible[String]] ==> Possible.Present("abc")
    Possible(Foo("a")).transformInto[Option[Bar]] ==> Option(Bar("a"))
    Option(Foo("a")).transformInto[Possible[Bar]] ==> Possible(Bar("a"))
    compileErrors("""Possible("foobar").into[None.type].transform""").check(
      "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] to scala.None",
      "scala.None",
      "  derivation from possible: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] to scala.None is not supported in Chimney!",
      "Consult https://chimney.readthedocs.io for usage examples."
    )
  }

  test("transform from non-OptionalValue into OptionalValue") {
    "abc".transformInto[Possible[String]] ==> Possible.Present("abc")
    (null: String).transformInto[Possible[String]] ==> Possible.Nope
  }

  test("transform into OptionalValue with an override") {
    Foo("abc").into[Possible[Foo]].withFieldConst(_.matchingSome.value, "def").transform ==> Possible(Foo("def"))
    Option(Foo("abc")).into[Possible[Foo]].withFieldConst(_.matchingSome.value, "def").transform ==> Possible(
      Foo("def")
    )
  }

  test("transform from TotallyBuildIterable to TotallyBuildIterable") {
    CustomCollection.of(Foo("a")).transformInto[Seq[Bar]] ==> Seq(Bar("a"))
    Seq(Foo("a")).transformInto[CustomCollection[Bar]] ==> CustomCollection.of(Bar("a"))
    CustomCollection.of(Foo("a")).transformInto[CustomCollection[Bar]] ==> CustomCollection.of(Bar("a"))
  }

  test("transform between Array-type and TotallyBuildIterable") {
    CustomCollection.of(Foo("a")).transformInto[Array[Bar]] ==> Array(Bar("a"))
    Array(Foo("a")).transformInto[CustomCollection[Bar]] ==> CustomCollection.of(Bar("a"))
  }

  test("read from PartiallyBuildIterable but not write to it") {
    NonEmptyCollection.of(Foo("a")).transformInto[CustomCollection[Bar]] ==> CustomCollection.of(Bar("a"))
    compileErrors("""CustomCollection.of(Foo("a")).transformInto[NonEmptyCollection[Bar]]""").check(
      "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.CustomCollection[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo] to io.scalaland.chimney.TotalTransformerIntegrationsSpec.NonEmptyCollection[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar]",
      "io.scalaland.chimney.TotalTransformerIntegrationsSpec.NonEmptyCollection[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar]",
      "  derivation from customcollection: io.scalaland.chimney.TotalTransformerIntegrationsSpec.CustomCollection[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo] to io.scalaland.chimney.TotalTransformerIntegrationsSpec.NonEmptyCollection[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar] is not supported in Chimney!",
      "Consult https://chimney.readthedocs.io for usage examples."
    )
  }

  test("transform into sequential type with an override") {
    Seq(Foo("a"))
      .into[CustomCollection[Bar]]
      .withFieldConst(_.everyItem.value, "b")
      .transform ==> CustomCollection.of(Bar("b"))
    CustomCollection
      .of(Foo("a"))
      .into[CustomCollection[Bar]]
      .withFieldConst(_.everyItem.value, "b")
      .transform ==> CustomCollection.of(Bar("b"))
  }

  test("transform from TotallyBuildMap to TotallyBuildMap") {
    CustomMap.of(Foo("k") -> Foo("v")).transformInto[Map[Bar, Bar]] ==> Map(Bar("k") -> Bar("v"))
    Map(Foo("k") -> Foo("v")).transformInto[CustomMap[Bar, Bar]] ==> CustomMap.of(Bar("k") -> Bar("v"))
    CustomMap.of(Foo("k") -> Foo("v")).transformInto[CustomMap[Bar, Bar]] ==> CustomMap.of(Bar("k") -> Bar("v"))
  }

  test("transform between TotallyBuildIterable and TotallyBuildMap") {
    CustomMap
      .of(Foo("k") -> Foo("v"))
      .transformInto[CustomCollection[(Bar, Bar)]] ==> CustomCollection.of(Bar("k") -> Bar("v"))
    CustomCollection
      .of(Foo("k") -> Foo("v"))
      .transformInto[CustomMap[Bar, Bar]] ==> CustomMap.of(Bar("k") -> Bar("v"))
  }

  test("read from PartiallyBuildIterable but not write to it") {
    NonEmptyMap.of(Foo("k") -> Foo("v")).transformInto[CustomMap[Bar, Bar]] ==> CustomMap.of(Bar("k") -> Bar("v"))
    compileErrors("""CustomMap.of(Foo("k") -> Foo("v")).transformInto[NonEmptyMap[Bar, Bar]]""").check(
      "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.CustomMap[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo, io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo] to io.scalaland.chimney.TotalTransformerIntegrationsSpec.NonEmptyMap[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar, io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar]",
      "io.scalaland.chimney.TotalTransformerIntegrationsSpec.NonEmptyMap[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar, io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar]",
      "  derivation from custommap: io.scalaland.chimney.TotalTransformerIntegrationsSpec.CustomMap[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo, io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo] to io.scalaland.chimney.TotalTransformerIntegrationsSpec.NonEmptyMap[io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar, io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Bar] is not supported in Chimney!",
      "Consult https://chimney.readthedocs.io for usage examples."
    )
  }

  test("transform into map type with an override") {
    Map(Foo("k") -> Foo("v"))
      .into[CustomMap[Bar, Bar]]
      .withFieldConst(_.everyMapKey.value, "k2")
      .withFieldConst(_.everyMapValue.value, "v2")
      .transform ==> CustomMap.of(Bar("k2") -> Bar("v2"))
    CustomMap
      .of(Foo("k") -> Foo("v"))
      .into[CustomMap[Bar, Bar]]
      .withFieldConst(_.everyMapKey.value, "k2")
      .withFieldConst(_.everyMapValue.value, "v2")
      .transform ==> CustomMap.of(Bar("k2") -> Bar("v2"))
  }

  group("flag .enableOptionDefaultsToNone") {

    case class Source(x: String)
    case class TargetWithOption(x: String, y: Possible[Int])
    case class TargetWithOptionAndDefault(x: String, y: Possible[Int] = Possible.Present(42))

    test("should be turned off by default and not allow compiling OptionalValue fields with missing source") {
      compileErrors("""Source("foo").into[TargetWithOption].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source to io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "  y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source",
        "There are default optional values available for y, the constructor argument/setter in io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption. Consider using .enableOptionDefaultsToNone.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("use OptionalValue.empty for fields without source nor default value when enabled") {
      Source("foo")
        .into[TargetWithOption]
        .enableOptionDefaultsToNone
        .transform ==> TargetWithOption("foo", Possible.Nope)
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

    test(
      "use OptionalValue.empty for fields without source but with default value when enabled only for a single field when scoped using .withTargetFlag(_.field)"
    ) {
      Source("foo")
        .into[TargetWithOptionAndDefault]
        .withTargetFlag(_.y)
        .enableOptionDefaultsToNone
        .transform ==> TargetWithOptionAndDefault("foo", Possible.Nope)
    }
  }

  group("flag .disableOptionDefaultsToNone") {

    @unused case class Source(x: String)
    @unused case class TargetWithOption(x: String, y: Possible[Int])

    test("should disable globally enabled .enableOptionDefaultsToNone") {
      @unused implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

      compileErrors("""Source("foo").into[TargetWithOption].disableOptionDefaultsToNone.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source to io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "  y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source",
        "There are default optional values available for y, the constructor argument/setter in io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption. Consider using .enableOptionDefaultsToNone.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }
}
object TotalTransformerIntegrationsSpec {

  import integrations.*

  class NonEmptyWrapper[A] private (val head: A, val tail: Set[A]) {
    def add(a: A): NonEmptyWrapper[A] = if (tail(a)) this else new NonEmptyWrapper(head, tail + a)

    override def equals(obj: Any): Boolean = obj match {
      case wrapper: NonEmptyWrapper[?] => (tail + head) == (wrapper.tail + wrapper.head)
      case _                           => false
    }
  }
  object NonEmptyWrapper {
    def apply[A](a: A, as: A*): NonEmptyWrapper[A] = new NonEmptyWrapper[A](a, as.toSet)
  }
  class SortedWrapper[A] private (val set: SortedSet[A]) {
    def add(a: A): SortedWrapper[A] = new SortedWrapper(set + a)

    override def equals(obj: Any): Boolean = obj match {
      case wrapper: SortedWrapper[?] => set == wrapper.set
      case _                         => false
    }
  }
  object SortedWrapper {
    def apply[A: Ordering](as: A*): SortedWrapper[A] = new SortedWrapper(SortedSet[A](as*))
  }

  object OuterTransformers {

    implicit def totalNonEmptyToSorted[A, B: Ordering]
        : integrations.TotalOuterTransformer[NonEmptyWrapper[A], SortedWrapper[B], A, B] =
      new integrations.TotalOuterTransformer[NonEmptyWrapper[A], SortedWrapper[B], A, B] {

        def transformWithTotalInner(
            src: NonEmptyWrapper[A],
            inner: A => B
        ): SortedWrapper[B] = SortedWrapper((src.tail + src.head).map(inner).toSeq*)

        def transformWithPartialInner(
            src: NonEmptyWrapper[A],
            failFast: Boolean,
            inner: A => partial.Result[B]
        ): partial.Result[SortedWrapper[B]] = partial.Result
          .traverse[Seq[B], A, B]((src.tail + src.head).iterator, inner, failFast)
          .map(bs => SortedWrapper[B](bs*))
      }
    implicit def partialNonEmptyToSorted[A, B: Ordering]
        : integrations.PartialOuterTransformer[NonEmptyWrapper[A], SortedWrapper[B], A, B] =
      new integrations.PartialOuterTransformer[NonEmptyWrapper[A], SortedWrapper[B], A, B] {

        def transformWithTotalInner(
            src: NonEmptyWrapper[A],
            failFast: Boolean,
            inner: A => B
        ): partial.Result[SortedWrapper[B]] =
          partial.Result.fromValue(SortedWrapper((src.tail + src.head).map(inner).toSeq*))

        def transformWithPartialInner(
            src: NonEmptyWrapper[A],
            failFast: Boolean,
            inner: A => partial.Result[B]
        ): partial.Result[SortedWrapper[B]] =
          partial.Result
            .traverse[Seq[B], A, B]((src.tail + src.head).iterator, inner, failFast)
            .map(bs => SortedWrapper[B](bs*))
      }
    implicit def partialSortedToNonEmpty[A, B]
        : integrations.PartialOuterTransformer[SortedWrapper[A], NonEmptyWrapper[B], A, B] =
      new integrations.PartialOuterTransformer[SortedWrapper[A], NonEmptyWrapper[B], A, B] {

        def transformWithTotalInner(
            src: SortedWrapper[A],
            failFast: Boolean,
            inner: A => B
        ): partial.Result[NonEmptyWrapper[B]] = src.set.toList.map(inner) match {
          case head :: tail => partial.Result.fromValue(NonEmptyWrapper(head, tail.toSeq*))
          case _            => partial.Result.fromEmpty
        }

        def transformWithPartialInner(
            src: SortedWrapper[A],
            failFast: Boolean,
            inner: A => partial.Result[B]
        ): partial.Result[NonEmptyWrapper[B]] =
          partial.Result.traverse[List[B], A, B](src.set.iterator, inner, failFast).flatMap {
            case head :: tail => partial.Result.fromValue(NonEmptyWrapper(head, tail.toSeq*))
            case _            => partial.Result.fromEmpty
          }
      }
  }

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
    override def toString(): String = s"CustomCollection(${impl.mkString(", ")})"
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

    def toVector: Vector[(K, V)] = impl

    override def equals(obj: Any): Boolean = obj match {
      case customMap: CustomMap[?, ?] => impl == customMap.impl
      case _                          => false
    }
    override def hashCode(): Int = impl.hashCode()
    override def toString(): String = s"CustomMap(${impl.mkString(", ")})"
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
