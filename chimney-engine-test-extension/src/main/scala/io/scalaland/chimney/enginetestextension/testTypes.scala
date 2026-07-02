package io.scalaland.chimney.enginetestextension

/** Test-only types supported EXCLUSIVELY through [[EngineTestMacroExtension]] (a Hearth `StandardMacroExtension`
  * registered via `META-INF/services`), never through implicits nor chimney's hardcoded shapes:
  *   - [[TestWrapper]] - `IsValueType`: rejected by the engine's Method-based `WrapperClassType` parse (private
  *     constructor, getter name differs from the constructor argument),
  *   - [[TestCollection]] - `IsCollection`: not `<: Iterable`, so the hardcoded `IterableOrArray` shapes reject it,
  *   - [[TestDict]] - `IsMap` (an `IsCollection` provider returning `IsMapOf` with a custom [[TestEntry]] pair type),
  *   - [[TestPossible]] - `IsOption`: not `scala.Option`-shaped, so the built-in Option support rejects it.
  */
final class TestWrapper private (s: String) {
  def unwrap: String = s
  override def equals(obj: Any): Boolean = obj match {
    case other: TestWrapper => other.unwrap == unwrap
    case _                  => false
  }
  override def hashCode: Int = s.hashCode
  override def toString: String = s"TestWrapper($s)"
}
object TestWrapper {
  def wrap(value: String): TestWrapper = new TestWrapper(value)
}

final class TestCollection[A] private (val toVector: Vector[A]) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestCollection[?] => other.toVector == toVector
    case _                        => false
  }
  override def hashCode: Int = toVector.hashCode
  override def toString: String = toVector.mkString("TestCollection(", ", ", ")")
}
object TestCollection {
  def of[A](values: A*): TestCollection[A] = new TestCollection(values.toVector)
  def fromVector[A](values: Vector[A]): TestCollection[A] = new TestCollection(values)
}

/** Custom pair type (deliberately NOT `Tuple2` - mirrors `java.util.Map.Entry` so the engine's pair-to-tuple
  * adaptation in the IsMap fallback is exercised).
  */
final class TestEntry[K, V](val key: K, val value: V) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestEntry[?, ?] => other.key == key && other.value == value
    case _                      => false
  }
  override def hashCode: Int = (key, value).hashCode
  override def toString: String = s"TestEntry($key, $value)"
}

final class TestDict[K, V] private (val toEntryVector: Vector[TestEntry[K, V]]) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestDict[?, ?] => other.toEntryVector == toEntryVector
    case _                     => false
  }
  override def hashCode: Int = toEntryVector.hashCode
  override def toString: String = toEntryVector.mkString("TestDict(", ", ", ")")
}
object TestDict {
  def of[K, V](pairs: (K, V)*): TestDict[K, V] = new TestDict(
    pairs.toVector.map(pair => new TestEntry(pair._1, pair._2))
  )
  def fromEntryVector[K, V](entries: Vector[TestEntry[K, V]]): TestDict[K, V] = new TestDict(entries)
}

final class TestPossible[A] private (val toOption: Option[A]) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestPossible[?] => other.toOption == toOption
    case _                      => false
  }
  override def hashCode: Int = toOption.hashCode
  override def toString: String = toOption.fold("TestPossible.absent")(a => s"TestPossible.present($a)")
}
object TestPossible {
  def present[A](value: A): TestPossible[A] = new TestPossible(Some(value))
  def absent[A]: TestPossible[A] = new TestPossible(None)
  def fromOption[A](option: Option[A]): TestPossible[A] = new TestPossible(option)
}

/** Runtime helpers for [[EngineTestMacroExtension]]'s quotes (the "runtime helper pattern" from the Kindlings
  * `hearth-collection-map` skill).
  *
  * HEARTH GOTCHA (report upstream, Scala 2 only): a cross-quote in a SEPARATELY COMPILED extension module that
  * references a companion object of a class (e.g. `TestWrapper.wrap(...)`) produces a tree whose qualifier symbol
  * resolves to the CLASS instead of the MODULE at the downstream macro-expansion site - scalac then fails with "value
  * wrap is not a member of ...TestWrapper (did you mean unwrap?)". An object WITHOUT a companion class (like this one)
  * is immune. (Scala 3 handles the companion references fine.)
  */
object testsupport {
  def wrapTestWrapper(value: String): TestWrapper = TestWrapper.wrap(value)

  def testCollectionFactory[A]: scala.collection.Factory[A, TestCollection[A]] =
    new scala.collection.Factory[A, TestCollection[A]] {
      def fromSpecific(it: IterableOnce[A]): TestCollection[A] = TestCollection.fromVector(Vector.from(it))
      def newBuilder: scala.collection.mutable.Builder[A, TestCollection[A]] =
        Vector.newBuilder[A].mapResult(vector => TestCollection.fromVector(vector))
    }

  def testDictFactory[K, V]: scala.collection.Factory[TestEntry[K, V], TestDict[K, V]] =
    new scala.collection.Factory[TestEntry[K, V], TestDict[K, V]] {
      def fromSpecific(it: IterableOnce[TestEntry[K, V]]): TestDict[K, V] =
        TestDict.fromEntryVector(Vector.from(it))
      def newBuilder: scala.collection.mutable.Builder[TestEntry[K, V], TestDict[K, V]] =
        Vector.newBuilder[TestEntry[K, V]].mapResult(entries => TestDict.fromEntryVector(entries))
    }

  def testEntry[K, V](key: K, value: V): TestEntry[K, V] = new TestEntry(key, value)

  def testPossiblePresent[A](value: A): TestPossible[A] = TestPossible.present(value)
  def testPossibleAbsent[A]: TestPossible[A] = TestPossible.absent[A]
  def testPossibleFromOption[A](option: Option[A]): TestPossible[A] = TestPossible.fromOption(option)
}
