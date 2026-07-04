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

/** Custom pair type (deliberately NOT `Tuple2` - mirrors `java.util.Map.Entry` so the engine's pair-to-tuple adaptation
  * in the IsMap fallback is exercised).
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

/** Non-empty collection supported EXCLUSIVELY through a SMART-CONSTRUCTOR `IsCollection` provider whose shape mirrors
  * Kindlings cats-integration's `NonEmptyList` provider 1:1 (`CtorResult = List[A]`, `build` =
  * `CtorLikeOf.EitherStringOrValue` returning `Left("Cannot create ... from empty collection")`).
  */
final class TestNonEmptyCollection[A] private (val toList: List[A]) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestNonEmptyCollection[?] => other.toList == toList
    case _                                => false
  }
  override def hashCode: Int = toList.hashCode
  override def toString: String = toList.mkString("TestNonEmptyCollection(", ", ", ")")
}
object TestNonEmptyCollection {
  def of[A](head: A, tail: A*): TestNonEmptyCollection[A] = new TestNonEmptyCollection(head :: tail.toList)
  def fromList[A](values: List[A]): Either[String, TestNonEmptyCollection[A]] =
    if (values.nonEmpty) Right(new TestNonEmptyCollection(values))
    else Left("Cannot create TestNonEmptyCollection from empty collection")
}

/** Non-empty dictionary supported EXCLUSIVELY through a SMART-CONSTRUCTOR `IsMap` provider (mirrors Kindlings'
  * `NonEmptyMap` shape but with the custom [[TestEntry]] pair type, so the partial pair-to-tuple adaptation is
  * exercised too).
  */
final class TestNonEmptyDict[K, V] private (val toEntryList: List[TestEntry[K, V]]) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestNonEmptyDict[?, ?] => other.toEntryList == toEntryList
    case _                             => false
  }
  override def hashCode: Int = toEntryList.hashCode
  override def toString: String = toEntryList.mkString("TestNonEmptyDict(", ", ", ")")
}
object TestNonEmptyDict {
  def of[K, V](head: (K, V), tail: (K, V)*): TestNonEmptyDict[K, V] = new TestNonEmptyDict(
    (head +: tail.toList).map(pair => new TestEntry(pair._1, pair._2))
  )
  def fromEntryList[K, V](entries: List[TestEntry[K, V]]): Either[String, TestNonEmptyDict[K, V]] =
    if (entries.nonEmpty) Right(new TestNonEmptyDict(entries))
    else Left("Cannot create TestNonEmptyDict from empty collection")
}

/** Smart-constructor value types - one per validated `CtorLikeOf` shape (their `IsValueType` providers' `wrap` uses
  * `EitherStringOrValue`/`EitherIterableStringOrValue`/`EitherThrowableOrValue`/`EitherIterableThrowableOrValue`
  * respectively). All wrap a `String` that must be non-empty (the `Multi`/`Throwables` ones additionally reject digits,
  * so a single input can produce MULTIPLE errors).
  */
final class TestSmartWrapper private (val value: String) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestSmartWrapper => other.value == value
    case _                       => false
  }
  override def hashCode: Int = value.hashCode
  override def toString: String = s"TestSmartWrapper($value)"
}
object TestSmartWrapper {
  def parse(value: String): Either[String, TestSmartWrapper] =
    if (value.nonEmpty) Right(new TestSmartWrapper(value)) else Left("TestSmartWrapper cannot be empty")
  def unsafe(value: String): TestSmartWrapper = new TestSmartWrapper(value)
}

final class TestSmartWrapperMulti private (val value: String) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestSmartWrapperMulti => other.value == value
    case _                            => false
  }
  override def hashCode: Int = value.hashCode
  override def toString: String = s"TestSmartWrapperMulti($value)"
}
object TestSmartWrapperMulti {
  def parse(value: String): Either[Iterable[String], TestSmartWrapperMulti] = {
    val errors = List(
      if (value.length < 2) List("TestSmartWrapperMulti is too short") else Nil,
      if (value.exists(_.isDigit)) List("TestSmartWrapperMulti contains digits") else Nil
    ).flatten
    if (errors.isEmpty) Right(new TestSmartWrapperMulti(value)) else Left(errors)
  }
  def unsafe(value: String): TestSmartWrapperMulti = new TestSmartWrapperMulti(value)
}

final class TestSmartWrapperThrowable private (val value: String) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestSmartWrapperThrowable => other.value == value
    case _                                => false
  }
  override def hashCode: Int = value.hashCode
  override def toString: String = s"TestSmartWrapperThrowable($value)"
}
object TestSmartWrapperThrowable {
  def parse(value: String): Either[Throwable, TestSmartWrapperThrowable] =
    if (value.nonEmpty) Right(new TestSmartWrapperThrowable(value))
    else Left(new IllegalArgumentException("TestSmartWrapperThrowable cannot be empty"))
  def unsafe(value: String): TestSmartWrapperThrowable = new TestSmartWrapperThrowable(value)
}

final class TestSmartWrapperThrowables private (val value: String) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestSmartWrapperThrowables => other.value == value
    case _                                 => false
  }
  override def hashCode: Int = value.hashCode
  override def toString: String = s"TestSmartWrapperThrowables($value)"
}
object TestSmartWrapperThrowables {
  def parse(value: String): Either[Iterable[Throwable], TestSmartWrapperThrowables] = {
    val errors = List(
      if (value.length < 2) List(new IllegalArgumentException("TestSmartWrapperThrowables is too short")) else Nil,
      if (value.exists(_.isDigit)) List(new IllegalArgumentException("TestSmartWrapperThrowables contains digits"))
      else Nil
    ).flatten
    if (errors.isEmpty) Right(new TestSmartWrapperThrowables(value)) else Left(errors)
  }
  def unsafe(value: String): TestSmartWrapperThrowables = new TestSmartWrapperThrowables(value)
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

/** Runtime helpers for [[EngineTestMacroExtension]]'s quotes - the "runtime helper pattern" (multi-line factory logic
  * lives here rather than inline in quotes).
  *
  * Historical note: this used to ALSO be a workaround for [[https://github.com/kubuszok/hearth/issues/320 hearth#320]]
  * (Scala 2 cross-unit quotes mis-resolving companion-object references - "value wrap is not a member of
  * ...TestWrapper"), fixed in Hearth 0.4.1; [[EngineTestMacroExtension]]'s `TestWrapper.wrap` quote now calls the
  * companion DIRECTLY to pin the fix, the rest keep the helper style.
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

  // Smart-constructor helpers (companion calls routed through this companion-less object - Scala 2 gotcha above).

  def listFactory[A]: scala.collection.Factory[A, List[A]] =
    new scala.collection.Factory[A, List[A]] {
      def fromSpecific(it: IterableOnce[A]): List[A] = List.from(it)
      def newBuilder: scala.collection.mutable.Builder[A, List[A]] = List.newBuilder[A]
    }

  def entryListFactory[K, V]: scala.collection.Factory[TestEntry[K, V], List[TestEntry[K, V]]] =
    new scala.collection.Factory[TestEntry[K, V], List[TestEntry[K, V]]] {
      def fromSpecific(it: IterableOnce[TestEntry[K, V]]): List[TestEntry[K, V]] = List.from(it)
      def newBuilder: scala.collection.mutable.Builder[TestEntry[K, V], List[TestEntry[K, V]]] =
        List.newBuilder[TestEntry[K, V]]
    }

  def buildTestNonEmptyCollection[A](values: List[A]): Either[String, TestNonEmptyCollection[A]] =
    TestNonEmptyCollection.fromList(values)

  def buildTestNonEmptyDict[K, V](entries: List[TestEntry[K, V]]): Either[String, TestNonEmptyDict[K, V]] =
    TestNonEmptyDict.fromEntryList(entries)

  def parseTestSmartWrapper(value: String): Either[String, TestSmartWrapper] =
    TestSmartWrapper.parse(value)
  def parseTestSmartWrapperMulti(value: String): Either[Iterable[String], TestSmartWrapperMulti] =
    TestSmartWrapperMulti.parse(value)
  def parseTestSmartWrapperThrowable(value: String): Either[Throwable, TestSmartWrapperThrowable] =
    TestSmartWrapperThrowable.parse(value)
  def parseTestSmartWrapperThrowables(value: String): Either[Iterable[Throwable], TestSmartWrapperThrowables] =
    TestSmartWrapperThrowables.parse(value)
}
