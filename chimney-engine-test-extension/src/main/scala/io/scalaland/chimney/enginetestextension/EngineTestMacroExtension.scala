package io.scalaland.chimney.enginetestextension

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

/** Test-only Hearth `StandardMacroExtension` registered via `META-INF/services/hearth.std.StandardMacroExtension`.
  *
  * Registers `IsValueType`/`IsCollection`/`IsOption` providers for [[TestWrapper]]/[[TestCollection]]/[[TestPossible]]
  * so chimney's engine tests can prove that ServiceLoader-registered extensions are consulted as the engine's built-in
  * FALLBACK layer (below user implicits and below `io.scalaland.chimney.integrations` implicits).
  *
  * Implementation notes (see the Kindlings `hearth-collection-map`/`hearth-value-types` skill patterns):
  *   - all cross-quotes live in helper methods with REGULAR type parameters (path-dependent types from
  *     `Type.Ctor1.unapply` inside `Expr.quote` break Scala 2's reifier),
  *   - generic type constructors are matched with `Type.Ctor1.fromUntyped` (`=:=` + `baseType`-aware, safe across
  *     compilation-unit boundaries - this module is compiled separately from the specs that trigger expansion),
  *   - the `IsValueType` provider sets `method = None` on its `CtorLikeOf.PlainValue`, so chimney's fallback derives
  *     the `WrapperClass.fieldName` default `"value"`.
  */
final class EngineTestMacroExtension extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      private lazy val TestWrapperType = Type.of[TestWrapper]
      private lazy val StringType = Type.of[String]

      @scala.annotation.nowarn("msg=is never used")
      private def testWrapperSupport: IsValueType[TestWrapper] = {
        implicit val A: Type[TestWrapper] = TestWrapperType
        implicit val S: Type[String] = StringType
        val plainValue = CtorLikeOf.PlainValue[String, TestWrapper](
          // Companion calls go through `testsupport` - see its ScalaDoc (Scala 2 companion-reference gotcha).
          ctor = (inner: Expr[String]) => Expr.quote(io.scalaland.chimney.enginetestextension.testsupport.wrapTestWrapper(Expr.splice(inner))),
          method = None
        )
        Existential[IsValueTypeOf[TestWrapper, *], String](new IsValueTypeOf[TestWrapper, String] {
          override val unwrap: Expr[TestWrapper] => Expr[String] =
            wrapped => Expr.quote(Expr.splice(wrapped).unwrap)
          override val wrap: CtorLikeOf[String, TestWrapper] = plainValue
          override lazy val ctors: CtorLikes[TestWrapper] =
            NonEmptyList.one(Existential[CtorLikeOf[*, TestWrapper], String](plainValue))
        })
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        if (tpe =:= TestWrapperType) ProviderResult.Matched(testWrapperSupport.asInstanceOf[IsValueType[A]])
        else skipped(s"${tpe.prettyPrint} is not TestWrapper")
    })

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

      private lazy val TestCollectionCtor =
        Type.Ctor1.fromUntyped[TestCollection](Type.Ctor1.of[TestCollection].asUntyped)

      @scala.annotation.nowarn("msg=is never used")
      private def testCollectionSupport[Item0](item: Type[Item0]): IsCollection[TestCollection[Item0]] = {
        implicit val Item: Type[Item0] = item
        implicit val Coll: Type[TestCollection[Item0]] = TestCollectionCtor[Item0]
        implicit val IterableItem: Type[Iterable[Item0]] = Type.of[Iterable[Item0]]
        implicit val FactoryType: Type[scala.collection.Factory[Item0, TestCollection[Item0]]] =
          Type.of[scala.collection.Factory[Item0, TestCollection[Item0]]]
        implicit val BuilderType: Type[scala.collection.mutable.Builder[Item0, TestCollection[Item0]]] =
          Type.of[scala.collection.mutable.Builder[Item0, TestCollection[Item0]]]
        // All quotes are prepared OUTSIDE the anonymous class: inside it the (implicit) `CtorResult` member would be
        // ambiguous with `Coll` when cross-quotes resolve `Type[TestCollection[Item0]]`.
        val asIterableFn: Expr[TestCollection[Item0]] => Expr[Iterable[Item0]] =
          value => Expr.quote(Expr.splice(value).toVector)
        val factoryExpr: Expr[scala.collection.Factory[Item0, TestCollection[Item0]]] =
          Expr.quote(io.scalaland.chimney.enginetestextension.testsupport.testCollectionFactory[Item0])
        val buildCtor
            : CtorLikeOf[scala.collection.mutable.Builder[Item0, TestCollection[Item0]], TestCollection[Item0]] =
          CtorLikeOf.PlainValue(
            (builder: Expr[scala.collection.mutable.Builder[Item0, TestCollection[Item0]]]) =>
              Expr.quote(Expr.splice(builder).result()),
            None
          )
        Existential[IsCollectionOf[TestCollection[Item0], *], Item0](
          new IsCollectionOf[TestCollection[Item0], Item0] {
            override def asIterable(value: Expr[TestCollection[Item0]]): Expr[Iterable[Item0]] =
              asIterableFn(value)
            override type CtorResult = TestCollection[Item0]
            implicit override val CtorResult: Type[CtorResult] = Coll
            override def factory: Expr[scala.collection.Factory[Item0, CtorResult]] = factoryExpr
            override def build
                : CtorLikeOf[scala.collection.mutable.Builder[Item0, CtorResult], TestCollection[Item0]] = buildCtor
          }
        )
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = tpe match {
        case TestCollectionCtor(item) if !(item.Underlying =:= Type.of[Nothing]) =>
          import item.Underlying as Item
          ProviderResult.Matched(testCollectionSupport[Item](Item).asInstanceOf[IsCollection[A]])
        case _ => skipped(s"${tpe.prettyPrint} is not TestCollection[_]")
      }
    })

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

      private lazy val TestDictCtor =
        Type.Ctor2.fromUntyped[TestDict](Type.Ctor2.of[TestDict].asUntyped)
      private lazy val TestEntryCtor =
        Type.Ctor2.fromUntyped[TestEntry](Type.Ctor2.of[TestEntry].asUntyped)

      @scala.annotation.nowarn("msg=is never used")
      private def testDictSupport[K0, V0](keyType: Type[K0], valueType: Type[V0]): IsCollection[TestDict[K0, V0]] = {
        implicit val K: Type[K0] = keyType
        implicit val V: Type[V0] = valueType
        implicit val Entry: Type[TestEntry[K0, V0]] = TestEntryCtor[K0, V0]
        implicit val Dict: Type[TestDict[K0, V0]] = TestDictCtor[K0, V0]
        implicit val IterableEntry: Type[Iterable[TestEntry[K0, V0]]] = Type.of[Iterable[TestEntry[K0, V0]]]
        implicit val FactoryType: Type[scala.collection.Factory[TestEntry[K0, V0], TestDict[K0, V0]]] =
          Type.of[scala.collection.Factory[TestEntry[K0, V0], TestDict[K0, V0]]]
        implicit val BuilderType: Type[scala.collection.mutable.Builder[TestEntry[K0, V0], TestDict[K0, V0]]] =
          Type.of[scala.collection.mutable.Builder[TestEntry[K0, V0], TestDict[K0, V0]]]
        // All quotes are prepared OUTSIDE the anonymous class - see testCollectionSupport for the rationale.
        val asIterableFn: Expr[TestDict[K0, V0]] => Expr[Iterable[TestEntry[K0, V0]]] =
          dict => Expr.quote(Expr.splice(dict).toEntryVector)
        val factoryExpr: Expr[scala.collection.Factory[TestEntry[K0, V0], TestDict[K0, V0]]] =
          Expr.quote(io.scalaland.chimney.enginetestextension.testsupport.testDictFactory[K0, V0])
        val buildCtor: CtorLikeOf[
          scala.collection.mutable.Builder[TestEntry[K0, V0], TestDict[K0, V0]],
          TestDict[K0, V0]
        ] = CtorLikeOf.PlainValue(
          (builder: Expr[scala.collection.mutable.Builder[TestEntry[K0, V0], TestDict[K0, V0]]]) =>
            Expr.quote(Expr.splice(builder).result()),
          None
        )
        val keyFn: Expr[TestEntry[K0, V0]] => Expr[K0] = entry => Expr.quote(Expr.splice(entry).key)
        val valueFn: Expr[TestEntry[K0, V0]] => Expr[V0] = entry => Expr.quote(Expr.splice(entry).value)
        val pairFn: (Expr[K0], Expr[V0]) => Expr[TestEntry[K0, V0]] =
          (key, value) => Expr.quote(io.scalaland.chimney.enginetestextension.testsupport.testEntry(Expr.splice(key), Expr.splice(value)))
        Existential[IsCollectionOf[TestDict[K0, V0], *], TestEntry[K0, V0]](
          new IsMapOf[TestDict[K0, V0], TestEntry[K0, V0]] {
            override def asIterable(value: Expr[TestDict[K0, V0]]): Expr[Iterable[TestEntry[K0, V0]]] =
              asIterableFn(value)
            override type CtorResult = TestDict[K0, V0]
            implicit override val CtorResult: Type[CtorResult] = Dict
            override def factory: Expr[scala.collection.Factory[TestEntry[K0, V0], CtorResult]] = factoryExpr
            override def build: CtorLikeOf[
              scala.collection.mutable.Builder[TestEntry[K0, V0], CtorResult],
              TestDict[K0, V0]
            ] = buildCtor
            override type Key = K0
            implicit override val Key: Type[Key] = keyType
            override type Value = V0
            implicit override val Value: Type[Value] = valueType
            override def key(pair: Expr[TestEntry[K0, V0]]): Expr[Key] = keyFn(pair)
            override def value(pair: Expr[TestEntry[K0, V0]]): Expr[Value] = valueFn(pair)
            override def pair(key: Expr[Key], value: Expr[Value]): Expr[TestEntry[K0, V0]] = pairFn(key, value)
          }
        )
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = tpe match {
        case TestDictCtor(k, v) =>
          import k.Underlying as K0, v.Underlying as V0
          ProviderResult.Matched(testDictSupport[K0, V0](K0, V0).asInstanceOf[IsCollection[A]])
        case _ => skipped(s"${tpe.prettyPrint} is not TestDict[_, _]")
      }
    })

    IsOption.registerProvider(new IsOption.Provider {

      override def name: String = loader.getClass.getName

      private lazy val TestPossibleCtor =
        Type.Ctor1.fromUntyped[TestPossible](Type.Ctor1.of[TestPossible].asUntyped)

      @scala.annotation.nowarn("msg=is never used")
      private def testPossibleSupport[Item0](item: Type[Item0]): IsOption[TestPossible[Item0]] = {
        implicit val Item: Type[Item0] = item
        implicit val Possible: Type[TestPossible[Item0]] = TestPossibleCtor[Item0]
        Existential[IsOptionOf[TestPossible[Item0], *], Item0](new IsOptionOf[TestPossible[Item0], Item0] {
          override def empty: Expr[TestPossible[Item0]] =
            Expr.quote(io.scalaland.chimney.enginetestextension.testsupport.testPossibleAbsent[Item0])
          override def of(value: Expr[Item0]): Expr[TestPossible[Item0]] =
            Expr.quote(io.scalaland.chimney.enginetestextension.testsupport.testPossiblePresent(Expr.splice(value)))
          override def fold[B: Type](
              option: Expr[TestPossible[Item0]]
          )(onEmpty: Expr[B], onSome: Expr[Item0] => Expr[B]): Expr[B] = Expr.quote {
            Expr.splice(option).toOption.fold(Expr.splice(onEmpty)) { (item: Item0) =>
              Expr.splice(onSome(Expr.quote(item)))
            }
          }
          override def getOrElse(option: Expr[TestPossible[Item0]])(default: Expr[Item0]): Expr[Item0] =
            Expr.quote(Expr.splice(option).toOption.getOrElse(Expr.splice(default)))
          override def orElse(
              option: Expr[TestPossible[Item0]]
          )(default: Expr[TestPossible[Item0]]): Expr[TestPossible[Item0]] =
            Expr.quote(
              io.scalaland.chimney.enginetestextension.testsupport
                .testPossibleFromOption(Expr.splice(option).toOption.orElse(Expr.splice(default).toOption))
            )
        })
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsOption[A]] = tpe match {
        case TestPossibleCtor(item) if !(item.Underlying =:= Type.of[Nothing]) =>
          import item.Underlying as Item
          ProviderResult.Matched(testPossibleSupport[Item](Item).asInstanceOf[IsOption[A]])
        case _ => skipped(s"${tpe.prettyPrint} is not TestPossible[_]")
      }
    })
  }
}
