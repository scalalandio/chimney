# Cookbook

Examples of various use cases already handled by Chimney.

## Reusing flags for several transformations/patchings

If we do not want to enable the same flag(s) in several places, we can define shared flag configuration as an implicit:

!!! example

    Scala 2

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    implicit val transformerCfg = TransformerConfiguration.default.enableMethodAccessors.enableMacrosLogging

    implicit val patcherCfg = PatcherConfiguration.default.ignoreNoneInPatch.enableMacrosLogging
    ```  

    Scala 3

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    transparent inline given TransformerConfiguration[?] =
      TransformerConfiguration.default.enableMethodAccessors.enableMacrosLogging

    transparent inline given PatcherConfiguration[?] =
      PatcherConfiguration.default.ignoreNoneInPatch.enableMacrosLogging
    ```  

!!! tip

    As we can see, on Scala 3 we can skip useless names, but we are required to provide the type. Since we want it to
    be inferred (for our convenience), we can use `transparent inline` to provide this time as a wildcard type but still
    let Scala figure it out.
    
!!! tip

    These configs will be shared by all derivations triggered in the scope that this `implicit`/`given` was defined.
    This includes automatic derivation as well, so summoning automatically derived Transformer would adhere to these
    flags.

!!! warning

    Since 0.8.0, Chimney assumes that you do NOT want to use the use implicit Transformer if you passed any `withField*`
    or `withCoproduct*` customization - using an implicit would not make it possible to do so. However, setting any flag
    with `enable*` or `disable*` would not prevent using implicit. So you could have situation like:
    
    ```scala
    implicit val foo2bar: Transformer[Foo, Bar] = ??? // stub for what is actually here
    foo.into[Bar].enableDefaultValues.transform // uses foo2bar ignoring flags
    ```
    
    Since 0.8.1, Chimney would ignore an implicit if any flag was explicitly used in `.into.transform`. Flags defined in
    an implicit `TransformerConfiguration` would be considerd new default settings in new derivations, but would not
    cause `.into.transform` to ignore an implicit if one is present. 

## Automatic, Semiautomatic and Inlined derivation

When you use the standard way of working with Chimney, but `import io.scalaland.chimney.dsl._`
you might notice that it is a very convenient approach, making a lot of things easy:

  - when you want to trivially convert `val from: From` into `To` you can do
    it with `from.transformInto[To]`
  - the code above would be able to map case classes recursively
  - if you wanted to provide some transformation to use either directly in this
    `.transformInto` or in some nesting, you can do it just by using implicits
  - if you wanted to generate this implicit you could use `Transformer.derive`
  - if you needed to customize the derivation you could us
    `Transformer.define.customisationMethod.buildTransformer` or
    `from.into[To].customisationMethod.transform`

However, sometimes you may want to restrict this behavior. It might be too easy to:

  - derive the same transformation again and again
  - define some customized `Transformer`, not import it by accident and still
    end up with the compiling code since Chimney could derive a new one on the spot

### Automatic vs semiautomatic

In other libraries this issue is addressed by providing 2 flavors of derivation:

  - automatic derivation: usually requires some `import library.auto._`, allows you
    to get a derived instance just by summoning it e.g. with `implicitly[TypeClass[A]]`
    or calling any other method that would take it as an `implicit` parameter.
  
    Usually, it is convenient to use, but has a downside of re-deriving the same instance
    each time you need it. Additionally, you cannot write
  
    !!! example
  
        ```scala
        implicit val typeclass: TypeClass[A] = implicitly[TypeClass[A]]
        ```
  
    since that generates circular dependency on a value initialization. This makes it hard
    to cache this instance in e.g. companion object. In some libraries, it also makes it hard
    to use automatic derivation to work with recursive data structures.

  - semiautomatic derivation: requires you to explicitly call some method that will provide
    a derived instance. It has the downside that for each instance that you would like to summon
    you need to manually derive and assign to an `implicit val` or `def`

    !!! example

        ```scala
        implicit val typeclass: TypeClass[A] = deriveTypeClass[A]
        ```

    However, it gives you certainty that each time you need an instance of a type class
    it will be the one you manually created. It reduces compile time, and makes it easier
    to limit the places where error can happen (if you reuse the same instance everywhere
    and there is a bug in an instance, there is only one place to look for it).

The last property is a reason many projects encourage the usage of semiautomatic derivation
and many libraries provide automatic derivation as a quick and dirty way of doing things
requiring an opt-in.

Chimney's defaults for (good) historical reasons mix these 2 modes (and one more, which
will describe in a moment), but it also allows you to selectively use these imports

!!! example

    ```scala
    import io.scalaland.chimney.auto._
    import io.scalaland.chimney.inlined._
    import io.scalaland.chimney.syntax._
    ```

instead of `io.scalaland.chimney.dsl` to achieve a similar behavior:

  - if you `import io.scalaland.chimney.syntax._` it will expose only extension
    methods working with type classes (`Transformer`, `PartialTransformer` and `Patcher`),
    but with no derivation

  - if you `import io.scalaland.chimney.auto._` it will only provide implicit instances
    generated through derivation.

    Semiautomatic derivation was available for a long time using methods:

    !!! example

        ```scala
        // Defaults only:
        Transformer.derive[From, To]
        PartialTransformer.derive[From, To]
        Patcher.derive[A, Patch]
        // Allows customization:
        Transformer.define[From, To].buildTransformer
        PartialTransformer.define[From, To].buildTransformer
        Patcher.define[A, Patch].buildPatcher
        ```

  - finally, there is `import io.scalaland.chimney.inlined._`. It provides extension methods:

    !!! example

        ```scala
        from.into[To].transform
        from.intoPartial[To].transform
        from.using[To].patch
        ```

    At the first glance, all they do is generate a customized type class before calling it, but
    what actually happens is that it generates an inlined expression, with no type class
    instantiation - if the user provided a type class for top-level or nested transformation it
    will be used, but wherever Chimney has to generate code ad hoc, it will generate inlined
    code. For that reason, this could be considered a third mode, one where generated code
    is non-reusable, but optimized to avoid any type class allocation and deferring
    `partial.Result` wrapping (in case of `PartialTransformer` s) as long as possible.

### Performance concerns

When Chimney derives an expression, whether that is an expression directly inlined at a call site
or as the body of the `transform`/`patch` method inside a type class instance, it attempts
to generate a fast code.

It contains special cases for `Option` s, `Either` s, it attempts to avoid boxing with
`partial.Result` and creating type classes if it can help it.

You can use [`.enableMacrosLogging`](troubleshooting.md#debugging-macros) to see the code generated by

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class Foo(baz: Foo.Baz)
    object Foo {
      case class Baz(a: String)
    }
    case class Bar(baz: Bar.Baz)
    object Bar {
      case class Baz(a: String)
    }

    Foo(Foo.Baz("string")).into[Bar].enableMacrosLogging.transform
    ```

The generated code (in the absence of implicits) should be

!!! example

    ```scala
    val foo = Foo(Foo.Baz("string"))
    new Bar(new Bar.Baz(foo.baz.a))
    ```

Similarly, when deriving a type class it would be

!!! example

    ```scala
    new Transformer[Foo, Bar] {
      def transform(foo: Foo): Bar =
        new Bar(new Bar.Baz(foo.baz.a))
    }
    ```

However, Chimney is only able to do it when given free rein. It checks
if the user provided an implicit, and if they did, it should be used instead.

In case of the automatic derivation, it means that every single branching
in the code - derivation for a field of a case class, or a subtype of a
sealed hierarchy - will trigger a macro, which may or may not succeed
and if it succeeds it will introduce an allocation.

When using `import io.scalaland.chimney.dsl._` this is countered by the usage of
a `Transformer.AutoDerived` as a supertype of `Transformer` - automatic
derivation upcast `Transformer` and recursive construction of an expression requires
a normal `Transformer` so automatic derivation is NOT triggered. Either the user provided
an implicit or there is none.

However, with `import io.scalaland.chimney.auto._` the same semantics as in other
libraries is used: implicit def returns `Transformer`, so if derivation with defaults
is possible it will always be triggered.

The matter is even more complex with `PartialTransformer` s - they look for both implicit
`Transformer` s as well as implicit `PartialTransformer` s (users can provide either or both).
With the automatic derivation both versions could always be available, so users need to always
provide `implicitConflictResolution` flag.

For the reasons above the recommendations are as follows:

  - if you care about performance use either inline derivation (for a one-time-usage) or
    semiautomatic derivation (`.derive`/`.define.build*` + `syntax._`)
  - only use `import auto._` when you want predictable behavior similar to other libraries
    (predictably bad)
  - if you use unit tests to ensure that your code does what it should and benchmarks to
    ensure it is reasonably fast keep on using `import dsl._`

## Bidirectional transformations

In some cases you might want to derive 2 transformations: from some type into another type and back. Most of the time,
such case appears not when you are using transformation on the spot, but when you need to derived in a semiautomatic
way.

!!! example "Isomorphic transformation"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.Transformer
    
    case class Foo(a: Int, b: String)
    case class Bar(b: String, a: Int)
    
    object Bar {
      implicit val fromFoo: Transformer[Foo, Bar] = Transformer.derive
      implicit val intoFoo: Transformer[Bar, Foo] = Transformer.derive
    }
    ```

!!! example "Domain model encoding/decoding"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.{PartialTransformer, Transformer}
    
    case class Domain(a: Int, b: String)
    case class Dto(b: Option[String], a: Option[Int])
    
    object Dto {
      implicit val fromDomain: Transformer[Domain, Dto] = Transformer.derive
      implicit val intoDomain: PartialTransformer[Dto, Domain] = PartialTransformer.derive
    }
    ```

To make things less annoying, in such cases you can use `Iso` (for bidirectional conversion that always succeeds)
or `Codec` (for bidirectional conversion which always succeeds in one way, but might need validation in another):

!!! example "Isomorphic transformation"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.Iso
    
    case class Foo(a: Int, b: String)
    case class Bar(b: String, a: Int)
    
    object Bar {
      implicit val iso: Iso[Foo, Bar] = Iso.derive
      // Provides:
      // - iso.first: Transformer[Foo, Bar]
      // - iso.second: Transformer[Bar, Foo]
      // both automatically unpacked when using the DSL.
    }
    ```

!!! example "Domain model encoding/decoding"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.Codec
    
    case class Domain(a: Int, b: String)
    case class Dto(b: Option[String], a: Option[Int])
    
    object Dto {
      implicit val codec: Codec[Domain, Dto] = Codec.derive
      // Provides:
      // - codec.encode: Transformer[Foo, Bar]
      // - codec.decode: PartialTransformer[Bar, Foo]
      // both automatically unpacked when using the DSL.
    }
    ```

Both `Iso` and `Codec` are only available through semiautomatic derivation. Currently, they only provide
`withFieldRenamed` value override and flags overrides. 

## Java collections' integration

If you need support for:

  - `java.util.Optional` and convert to/from it as if it was `scala.Option`
  - `java.util.Collection`/`java.lang.Iterable`/`java.util.Enumerable` and convert to/from it as if it was
    `scala.collection.IterableOnce` with a dedicated `Factory` (or `CanBuildFrom`)
  - `java.util.Map`/`java.util.Dictionary`/`java.util.Properties` and convert to/from `scala.collection.Map`
  - `java.util.stream`s and convert them to/from all sorts of Scala collections
  - `java.lang.Boolean`/`java.lang.Byte`/`java.lang.Char`/`java.lang.Int`/`java.lang.Long`/`java.lang.Long`/
    `java.lang.Short`/`java.lang.Float`/`java.lang.Double` and convert to/from its `scala` counterpart

then you can use one simple import to enable it:

!!! example

    ```scala
    //> using dep io.scalaland::chimney-java-collections::{{ chimney_version() }}
    import io.scalaland.chimney.javacollections._
    ```

!!! warning

    There is an important performance difference between Chimney conversion and `scala.jdk.converions`.
    
    While `asJava` and `asScala` attempt to be O(1) operations, by creating a cheap wrapper around the original
    collection, Chimney creates a full copy. It is the only way to
    
      - target various specific implementations of the target type
      - guarantee that you don't merely wrap a mutable type which could be mutated right after you wrap it 

## Cats integration

Cats integration module contains the following utilities:

  - conversions between `partial.Result`s and `Validated` (and `ValidatedNel`, `ValidatedNec`) data type allowing 
    e.g. to convert `PartialTransformer`'s result to `Validated`
  - instances for Chimney types (many combined into single implicit to prevent conflicts):
     - for `Transformer` type class:
        - `ArrowChoice[Transformer] & CommutativeArrow[Transformer]` (implementing also `Arrow`, `Choice`, `Category`,
          `Compose`, `Strong`, `Profunctor`)
        - `[Source] => Monad[Transformer[Source, *]] & CoflatMap[Transformer[Source, *]]`
          (implementing also `Monad`, `Applicative`, `Functor`)
        - `[Target] => Contravariant[Transformer[*, Target]]` (implementing also `Invariant`)
     - for `PartialTransformer` type class:
        - `ArrowChoice[PartialTransformer] & CommutativeArrow[PartialTransformer]` (implementing also `Arrow`, `Choice`,
          `Category`,`Compose`, `Strong`, `Profunctor`)
        - `[Source] => MonadError[PartialTransformer[Source, *], partial.Result.Errors] & CoflatMap[PartialTransformer[Source, *]] & Alternative[PartialTransformer[Source, *]]`
          (implementing also `Monad`, `Applicative`, `Functor`, `ApplicativeError`, `NonEmptyAlternative`, `MonoidK`,
          `SemigroupK`)
        - `[Source] => Parallel[PartialTransformer[Source, *]]` (implementing also `NonEmptyParallel`)
        - `[Target] => Contravariant[Transformer[*, Target]]` (implementing also `Invariant`)
     - for `partial.Result` data type:
        - `MonadError[partial.Result, partial.Result.Errors] & CoflatMap[partial.Result] & Traverse[partial.Result] $ Alternative[partial.Result]`
          (implementing also `Monad`, `Applicative`, `Functor`, `ApplicativeError`, `UnorderedTraverse`, `Foldable`,
          `UnorderedFoldable`, `Invariant`, `Semigriupal`, `NonEmptyAlternative`, `SemigroupK`, `MonoidK`)
        - `Parallel[partial.Result]` (implementing also`NonEmptyParallel`)
        - `Semigroup[partial.Result.Errors]`
     - for `Codec` type class:
        - `Category[Codec]`
        - `InvariantSemigroupal[Codec[Domain, *]]` (implementing also `Invariant`, `Semigroupal`)
     - for `Iso` type class:
        - `Category[Iso]`
        - `InvariantSemigroupal[Iso[First, *]]` (implementing also `Invariant`, `Semigroupal`)
  - instances for `cats.data` types allowing Chimney to recognize them as collections:
    - `cats.data.Chain` (transformation _from_ and _to_ always available)
    - `cats.data.NonEmptyChain` (transformations: _from_ always available, _to_ only with `PartialTransformer`)
    - `cats.data.NonEmptyLazyList` (transformation: _from_ always available, _to_ only with `PartialTransformer`,
      the type is only defined on 2.13+)
    - `cats.data.NonEmptyList` (transformation: _from_ always available, _to_ only with `PartialTransformer`)
    - `cats.data.NonEmptyMap` (transformation: _from_ always available, _to_ only with `PartialTransformer`)
    - `cats.data.NonEmptySeq` (transformation: _from_ always available, _to_ only with `PartialTransformer`)
    - `cats.data.NonEmptySet` (transformation: _from_ always available, _to_ only with `PartialTransformer`)
    - `cats.data.NonEmptyVector` (transformation: _from_ always available, _to_ only with `PartialTransformer`)

!!! important

    You need to import ``io.scalaland.chimney.cats._`` to have all of the above in scope.

### Conversion from/into Cats `Validated`

`Validated[E, A]` values can be converted into `partial.Result[A]` using `asResult` extension method, when their
`E` (`Invalid`) type is:

 - `partial.Result.Errors`
 - `NonEmptyChain[partial.Error]`
 - `NonEmptyList[partial.Error]`
 - `NonEmptyChain[String]`
 - `NonEmptyList[String]`

as soon as we import both `io.scalaland.chimney.partial.syntax._` (for extension method) and
`io.scalaland.chimney.cats._` (instances for `Validated`).

Just like you could run `asOption` or `asEither` on `PartialTransformer` result, you can convert to `Validated` with
new extension methods: `asValidatedNec`, `asValidatedNel`, `asValidatedChain` and `asValidatedList`.

!!! example

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class RegistrationForm(
        email: String,
        username: String,
        password: String,
        age: String
    )

    case class RegisteredUser(
        email: String,
        username: String,
        passwordHash: String,
        age: Int
    )

    import cats.data._
    import io.scalaland.chimney._
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.partial.syntax._
    import io.scalaland.chimney.cats._

    def hashpw(pw: String): String = s"trust me bro, $pw is hashed"

    def validateEmail(form: RegistrationForm): ValidatedNec[String, String] =
      if (form.email.contains('@')) {
        Validated.valid(form.email)
      } else {
        Validated.invalid(NonEmptyChain(s"${form.username}'s email: does not contain '@' character"))
      }

    def validateAge(form: RegistrationForm): ValidatedNec[String, Int] = form.age.toIntOption match {
      case Some(value) if value >= 18 => Validated.valid(value)
      case Some(value) => Validated.invalid(NonEmptyChain(s"${form.username}'s age: must have at least 18 years"))
      case None        => Validated.invalid(NonEmptyChain(s"${form.username}'s age: invalid number"))
    }

    implicit val partialTransformer: PartialTransformer[RegistrationForm, RegisteredUser] =
      PartialTransformer
        .define[RegistrationForm, RegisteredUser]
        .withFieldComputedPartial(_.email, form => validateEmail(form).asResult)
        .withFieldComputed(_.passwordHash, form => hashpw(form.password))
        .withFieldComputedPartial(_.age, form => validateAge(form).asResult)
        .buildTransformer

    val okForm = RegistrationForm("john@example.com", "John", "s3cr3t", "40")
    pprint.pprintln(
      okForm.transformIntoPartial[RegisteredUser].asValidatedNec
    )
    // expected output:
    // Valid(
    //   a = RegisteredUser(
    //     email = "john@example.com",
    //     username = "John",
    //     passwordHash = "trust me bro, s3cr3t is hashed",
    //     age = 40
    //   )
    // )

    pprint.pprintln(
      Array(
        RegistrationForm("john_example.com", "John", "s3cr3t", "10"),
        RegistrationForm("alice@example.com", "Alice", "s3cr3t", "19"),
        RegistrationForm("bob@example.com", "Bob", "s3cr3t", "21.5")
      ).transformIntoPartial[Array[RegisteredUser]].asValidatedNel
    )
    // expected output:
    // Invalid(
    //   e = NonEmptyList(
    //     head = Error(
    //       message = StringMessage(message = "John's email: does not contain '@' character"),
    //       path = Path(elements = List(Index(index = 0), Accessor(name = "email")))
    //     ),
    //     tail = List(
    //       Error(
    //         message = StringMessage(message = "John's age: must have at least 18 years"),
    //         path = Path(elements = List(Index(index = 0), Accessor(name = "age")))
    //       ),
    //       Error(
    //         message = StringMessage(message = "Bob's age: invalid number"),
    //         path = Path(elements = List(Index(index = 2), Accessor(name = "age")))
    //       )
    //     )
    //   )
    // )
    ```

    Form validation logic is implemented in terms of `Validated` data type. You can easily convert
    it to a `partial.Result` required by `withFieldComputedPartial` by just using `.toPartialResult`
    which is available after importing the cats integration utilities (`import io.scalaland.chimney.cats._`).
    
    Result of the partial transformation is then converted to `ValidatedNel` or `ValidatedNec` using either
    `.asValidatedNel` or `.asValidatedNec` extension method call.

### Cats instances

If you have the experience with Cats and their type classes, then behavior of `Transformer` needs no additional
explanation:

!!! example

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.cats._

    val example: Transformer[Int, String] = _.toString

    pprint.pprintln(
      example.map(str => s"valis is $str").transform(10)
    )
    pprint.pprintln(
      example.dimap[Double, String](_.toInt)(str => "value " + str).transform(10.50)
    )
    // example.contramap[Double](_.toInt).transform(10.50) // Scala has a problem inferring what is F and what is A here
    pprint.pprintln(
      cats.arrow.Arrow[Transformer].id[String].transform("value")
    )
    // expected output:
    // "valis is 10"
    // "value 10"
    // "value"
    ```

Similarly, there exists instances for `PartialTransformer` and `partial.Result`: 

!!! example

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.cats._

    val example: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

    pprint.pprintln(
      example.map(int => int.toDouble).transform("10")
    )
    pprint.pprintln(
      example.dimap[String, Float](str => str + "00")(int => int.toFloat).transform("10")
    )
    pprint.pprintln(
      cats.arrow.Arrow[PartialTransformer].id[String].transform("value")
    )
    // expected output:
    // Value(value = 10.0)
    // Value(value = 1000.0F)
    // Value(value = "value")
    ```

However, between Cats and Chimney, there is a difference in approach when it comes to "sequential" and "parallel"
computations.
 
!!! note

    For this explanation we are assuming there we are using some type which represents computations that can be
    interrupted - e.g. exception was thrown (`Try`, `cats.effect.IO`), we got `Left` value (`Either`) or `Invalid`
    (`Validated`). In Chimney such type is `partial.Result`.

For starters, let us remember how such distinction is handled in Cats.

!!! info "How it works in Cats" 

     - when we are using thing like e.g. `map`, `flatMap` to chain the operations if any of these operations "fail", we
       are not running the operations that would follow
     - so, `Monad` is usually representing some sequential computations, which (for types representing fallible
       computations) can "fail fast" or "short circuit"
       - since `Applicative` extends `Monad` and their behavior should be compatible (= applicative operations are
         implemented using `flatMap` under the hood) then e.g.
         `(NonEmptyList.of("error1").asLeft[Int], NonEmptyList.of("error2".asLeft[Int])).tupled` would contain only one
         error (the first one, `NonEmptyList.of("error1")`), even though we used the type which could aggregate them
     - for such cases - where we want to always use all partial results - Cats prepared `Parallel` type class which
       would allow us to `(NonEmptyList.of("error1").asLeft[Int], NonEmptyList.of("error2".asLeft[Int])).parTupled`
       which would combine the results
       - it's quite important to remember that `Parallel` here doesn't mean "asynchronous operations that run
         concurrently" but "failure in one result doesn't discard other results before we start to combine them"
     - long story short: when seeing `map`, `flatMap`, `tupled`, `mapN` we should expect "sequential" semantics which
       "fail fast" and for aggregation of errors requires "parallel" semantics with operations like `parTupled`,
       `parMapN`, `parFlatMapN` - the semantics is chosen at compile time by the choice of operator we used

    It makes perfect sense, because - while these type class do NOT have to be used with IO monads - these type classes
    CAN be used with IO monads, and so `map`, `flatMap`, `parMapN`, etc can be used to express the "structured
    concurrency" without introducing separate type classes. The kind of semantics we need is known upfront and
    hardcoded.

Now, let's take a look what Chimney does, and why the behavior is different.

!!! info "How it works in Chimney"

     - we have `partial.Result` data type which can aggregate multiple errors into one value
     - however, this aggregation is costly, so if you don't need it you can pass `failFast: Boolean` parameter to
       `PartialTransformer` (e.g. `from.transformIntoPartial[To](failFast = true)`) or `partial.Result` utilities
       (e.g. `partial.Result.traverse(coll.toIterator, a => ..., failFast = true)`)
     - it means that Chimney decides which semantics to use in runtime, with a flag

    And it makes sense for Chimney: during a type class derivation we do not select the semantics, because semantics chosen
    in one place, would not work in another forcing users to derive everything twice. It also allows us to pass this flag
    from some config or context and for better debugging experience (e.g. using fail fast for normal operations, but letting
    us change a switch in the deployed app without recompiling and redeploying everything to test just one call).

What does it mean to us?

!!! warning

    It means that every `PartialTransformer` which internally combines results from several smaller partial
    transformations has to have both semantics implemented internally and switch between them with a flag. If we combine
    `PartialTransformer`s using Cats' type classes and extension methods, the type class instance
    can:

     - pass the `failFast: Boolean` flag on
     - use the flag to decide on semantics when combinding several smaller `partial.Result`s
     - however, some operations like `mapN` use `flatMap` under the hood, so while the flag is propagated, some results
       can still be discarded, and e.g. `parProduct` or `parMapN` would have to be used NOT to use parallel semantics
       but to NOT disable parallel semantics for some transformations when we would pass `failFast = false` later on
    
    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.cats._

    val t0 = PartialTransformer.fromFunction[String, Int](_.toInt)
    val t1 = t0.map(_.toDouble)
    val t2 = t0.map(_.toLong)

    // uses 1 input value to create a tuple of 2 values, fails fast on the error for the first
    pprint.pprintln(
      t1.product(t2).transform("aa").asEitherErrorPathMessageStrings
    )
    // expected output:
    // Left(value = List(("", "For input string: \"aa\"")))

    // uses 1 input value to create a tuple of 2 values, agregates the errors for both
    pprint.pprintln(
      t1.parProduct(t2).transform("aa").asEitherErrorPathMessageStrings
    )
    // expected output:
    // Left(value = List(("", "For input string: \"aa\""), ("", "For input string: \"aa\"")))
    ```

    And `partial.Result`s have to use explicit combinators to decide whether it's sequential or parallel semantics:

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.cats._

    val result1 = partial.Result.fromErrorString[Int]("error 1")
    val result2 = partial.Result.fromErrorString[Double]("error 2")

    // all of these will preserve only the first error ("error 1"):
    pprint.pprintln(
      (result1, result2).mapN((a: Int, b: Double) => a + b) // partial.Result[Double]
    )
    pprint.pprintln(
      result1.product(result2) // partial.Result[(Int, Double)]
    )
    pprint.pprintln(
      result1 <* result2 // partial.Result[Int]
    )
    pprint.pprintln(
      result1 *> result2 // partial.Result[Double]
    )
    // expected output:
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List()))
    //   )
    // )

    // all of these will preserve both errors:
    pprint.pprintln(
      (result1, result2).parMapN((a: Int, b: Double) => a + b) // partial.Result[Double]
    )
    pprint.pprintln(
      result1.parProduct(result2) // partial.Result[(Int, Double)]
    )
    pprint.pprintln(
      result1 <& result2 // partial.Result[Int]
    )
    pprint.pprintln(
      result1 &> result2 // partial.Result[Double]
    )
    // expected output:
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List())),
    //     Error(message = StringMessage(message = "error 2"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List())),
    //     Error(message = StringMessage(message = "error 2"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List())),
    //     Error(message = StringMessage(message = "error 2"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List())),
    //     Error(message = StringMessage(message = "error 2"), path = Path(elements = List()))
    //   )
    // )
    ```

Notice that this is not an issue if we are transforming one value into another value in a non-fallible way, e.g. through
`map`, `contramap`, `dimap`. There is also no issie if we chain several `flatMap`s for something more like Kleisli
composition (`result.flatMap(f).flatMap(g)`) but becomes an issue when we use `flatMap` and `flatMap`-based operations
for building products (`result.flatMap(a => result2.map(b => (a, b))`).

## Protocol Buffers integration

Most of the time, working with Protocol Buffers should not be different from
working with any other DTO objects. `Transformer`s could be used to encode
domain objects into protobufs and `PartialTransformer`s could decode them.

However, there are 2 concepts specific to PBs and their implementation in
ScalaPB: storing unencoded values in an additional case class field and
wrapping done by sealed traits' cases in `oneof` values.

### `UnknownFieldSet`

By default, ScalaPB would generate in a case class an additional field
`unknownFields: UnknownFieldSet = UnknownFieldSet()`. This field
could be used if you want to somehow log/trace some extra values -
perhaps from another version of the schema - were passed but your current
version's parser did not need it.

The automatic conversion into a protobuf with such a field can be problematic:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    object scalapb {
      case class UnknownFieldSet()
    }

    object domain {
      case class Address(line1: String, line2: String)
    }
    object protobuf {
      case class Address(
        line1: String = "",
        line2: String = "",
        unknownFields: scalapb.UnknownFieldSet = scalapb.UnknownFieldSet()
      )
    }

    domain.Address("a", "b").transformInto[protobuf.Address]
    // expected error:
    // Chimney can't derive transformation from domain.Address to protobuf.Address
    //
    // protobuf.Address
    //   unknownFields: scalapb.UnknownFieldSet - no accessor named unknownFields in source type domain.Address
    //
    // Consult https://scalalandio.github.io/chimney for usage examples.
    ```

There are 2 ways in which Chimney could handle this issue:

  - using [default values](supported-transformations.md#allowing-fallback-to-the-constructors-default-values)
  
    !!! example "Globally enabled default values"
  
        ```scala
        domain
          .Address("a", "b")
          .into[protobuf.Address]
          .enableDefaultValues
          .transform

        // can be set up for the whole scope with: 
        // implicit val cfg = TransformerConfiguration.default.enableDefaultValues
        ```
  
    !!! example "Default values scoped only to UnknownFieldSet"
  
        ```scala
        domain
          .Address("a", "b")
          .into[protobuf.Address]
          .enableDefaultValueOfType[scalapb.UnknownFieldSet]
          .transform
        // can be set up for the whole scope with: 
        // implicit val cfg = TransformerConfiguration.default.enableDefaultValueOfType[scalapb.UnknownFieldSet]
        ```

  - manually [setting this one field](supported-transformations.md#wiring-the-constructors-parameter-to-a-provided-value)_

    !!! example

        ```scala
        domain
          .Address("a", "b")
          .into[protobuf.Address]
          .withFieldConst(_.unknownFields, UnknownFieldSet())
          .transform
        ```

However, if you have the control over the ScalaPB generation process, you could configure it
to simply not generate this field, either
by [editing the protobuf](https://scalapb.github.io/docs/customizations#file-level-options):

!!! example

    ```protobuf
    option (scalapb.options) = {
      preserve_unknown_fields: false
    };
    ```

or adding to [package-scoped options](https://scalapb.github.io/docs/customizations#package-scoped-options).
If the field won't be generated in the first place, there will be no issues with providing values to it.

At this point, one might also consider another option:

!!! example

    ```protobuf
    option (scalapb.options) = {
      no_default_values_in_constructor: true
    };
    ```

preventing ScalaBP from generating default values in constructor, to control
how exactly the protobuf value is created.

### `oneof` fields

`oneof` is a way in which Protocol Buffers allows using ADTs. The example PB:

!!! example

    ```protobuf
    message AddressBookType {
      message Public {}
      message Private {
        string owner = 1;
      }
      oneof value {
        Public public = 1;
        Private private = 2;
      }
    }
    ```

would generate scala code similar to (some parts removed for brevity):

!!! example

    ```scala
    package pb.addressbook

    final case class AddressBookType(
        value: AddressBookType.Value = AddressBookType.Value.Empty
    ) extends scalapb.GeneratedMessage
        with scalapb.lenses.Updatable[AddressBookType] {
      // ...
    }

    object AddressBookType extends scalapb.GeneratedMessageCompanion[AddressBookType] {
      sealed trait Value extends scalapb.GeneratedOneof
      object Value {
        case object Empty extends AddressBookType.Value {
          // ...
        }
        final case class Public(value: AddressBookType.Public) extends AddressBookType.Value {
          // ...
        }
        final case class Private(value: AddressBookType.Private) extends AddressBookType.Value {
          // ...
        }
      }
      final case class Public(
      ) extends scalapb.GeneratedMessage
          with scalapb.lenses.Updatable[Public] {}

      final case class Private(
          owner: _root_.scala.Predef.String = ""
      ) extends scalapb.GeneratedMessage
          with scalapb.lenses.Updatable[Private] {
        // ...
      }

      // ...
    }
    ```

As we can see:

  - there is an extra `Value.Empty` type
  - this is not a "flat" `sealed` hierarchy - `AddressBookType` wraps sealed hierarchy `AddressBookType.Value`,
    where each `case class` wraps the actual message

!!! warning

    This is the default output, there are 2 other (opt-in) possibilities described below!

Meanwhile, we would like to extract it into a flat:

!!! example

    ```scala
    package addressbook

    sealed trait AddressBookType
    object AddressBookType {
      case object Public extends AddressBookType
      case class Private(owner: String) extends AddressBookType
    }
    ```

Luckily for us, since 0.8.x Chimney supports
[automatic (un)wrapping of sealed hierarchy cases](supported-transformations.md#non-flat-adts).

Encoding (with `Transformer`s) is pretty straightforward:

!!! example

    ```scala
    import io.scalaland.chimney.dsl._

    val domainType: addressbook.AddressBookType = addressbook.AddressBookType.Private("test")
    val pbType: pb.addressbook.AddressBookType =
      pb.addressbook.AddressBookType.of(
        pb.addressbook.AddressBookType.Value.Private(
          pb.addressbook.AddressBookType.Private.of("test")
        )
      )

    domainType.into[pb.addressbook.AddressBookType.Value].transform == pbType.value
    ```

Decoding (with `PartialTransformer`s) requires handling of `Empty.Value` type

  - we can do it manually:
  
    !!! example
    
        ```scala
        import io.scalaland.chimney.dsl._

        pbType.value
          .intoPartial[addressbook.AddressBookType]
          .withSealedSubtypeHandledPartial[pb.addressbook.AddressBookType.Value.Empty.type](
            _ => partial.Result.fromEmpty
          )
          .transform
          .asOption == Some(domainType)
        ```

  - or handle all such fields with a single import:

    !!! example
  
        ```scala
        import io.scalaland.chimney.dsl._
        import io.scalaland.chimney.protobufs._ // includes support for empty scalapb.GeneratedMessage

        pbType.value.intoPartial[addressbook.AddressBookType].transform.asOption == Some(domainType)
        ```

!!! warning

    Importing `import io.scalaland.chimney.protobufs._` works only for the default output. If you used `sealed_value` or
    `sealed_value_optional` read further sections. 

### `sealed_value oneof` fields

In case we can edit our protobuf definition, we can arrange the generated code
to be a flat `sealed` hierarchy. It requires fulfilling [several conditions defined by ScalaPB](https://scalapb.github.io/docs/sealed-oneofs#sealed-oneof-rules).
For instance, the code below follows the mentioned requirements:

!!! example
  
    ```protobuf
    message CustomerStatus {
      oneof sealed_value {
        CustomerRegistered registered = 1;
        CustomerOneTime oneTime = 2;
      }
    }

    message CustomerRegistered {}
    message CustomerOneTime {}
    ```

and it would generate something like (again, some parts omitted for brevity):

!!! example
  
    ```scala
    package pb.order

    sealed trait CustomerStatus extends scalapb.GeneratedSealedOneof {
      type MessageType = CustomerStatusMessage
    }

    object CustomerStatus {
      case object Empty extends CustomerStatus
      sealed trait NonEmpty extends CustomerStatus
    }

    final case class CustomerRegistered(
    ) extends scalapb.GeneratedMessage
        with CustomerStatus.NonEmpty
        with scalapb.lenses.Updatable[CustomerRegistered] {
      // ...
    }

    final case class CustomerOneTime(
    ) extends scalapb.GeneratedMessage
        with CustomerStatus.NonEmpty
        with scalapb.lenses.Updatable[CustomerOneTime] {
      // ...
    }
    ```

Notice, that while this implementation is flat, it still adds `CustmerStatus.Empty` - it happens because this type
would be used directly inside the message that contains is, and it would be non-nullable (while the `oneof`
content could still be absent).

Transforming to and from:

!!! example
  
    ```scala
    package order

    sealed trait CustomerStatus
    object CustomerStatus {
      case object CustomerRegistered extends CustomerStatus
      case object CustomerOneTime extends CustomerStatus
    }
    ```

could be done with:

!!! example
  
    ```scala
    import io.scalaland.chimney.dsl._

    val domainStatus: order.CustomerStatus = order.CustomerStatus.CustomerRegistered
    val pbStatus: pb.order.CustomerStatus = pb.order.CustomerRegistered()

    domainStatus.into[pb.order.CustomerStatus].transform == pbStatus

    pbStatus
      .intoPartial[order.CustomerStatus]
      .withSealedSubtypeHandledPartial[pb.order.CustomerStatus.Empty.type](
        _ => partial.Result.fromEmpty
      )
      .withSealedSubtypeHandled[pb.order.CustomerStatus.NonEmpty](
        _.transformInto[order.CustomerStatus]
      )
      .transform
      .asOption == Some(domainStatus)
    ```

As you can see, we have to manually handle decoding the `Empty` value.

### `sealed_value_optional oneof` fields

If instead of a non-nullable type with `.Empty` subtype, we prefer `Option`al type without `.Empty` subtype, there is
an optional sealed hierarchy available. Similarly to a non-optional it requires [several conditions](https://scalapb.github.io/docs/sealed-oneofs#optional-sealed-oneof).

When you define message according to them:

!!! example
  
    ```protobuf
    message PaymentStatus {
      oneof sealed_value_optional {
        PaymentRequested requested = 1;
        PaymentCreated created = 2;
        PaymentSucceeded succeeded = 3;
        PaymentFailed failed = 4;
      }
    }

    message PaymentRequested {}
    message PaymentCreated {
      string external_id = 1;
    }
    message PaymentSucceeded {}
    message PaymentFailed {}
    ```

and try to map it to and from:

!!! example
  
    ```scala
    package order

    sealed trait PaymentStatus
    object PaymentStatus {
      case object PaymentRequested extends PaymentStatus
      case class PaymentCreated(externalId: String) extends PaymentStatus
      case object PaymentSucceeded extends PaymentStatus
      case object PaymentFailed extends PaymentStatus
    }
    ```

the transformation is pretty straightforward in both directions:

!!! example
  
    ```scala
    val domainStatus: Option[order.PaymentStatus] = Option(order.PaymentStatus.PaymentRequested)
    val pbStatus: Option[pb.order.PaymentStatus] = Option(pb.order.PaymentRequested())

    domainStatus.into[Option[pb.order.PaymentStatus]].transform ==> pbStatus
    pbStatus.into[Option[order.PaymentStatus]].transform ==> domainStatus
    ```

since there is no `Empty` case to handle. Wrapping into `Option` would
be handled automatically, similarly unwrapping (as long as you decode using
partial transformers).

### Build-in ScalaPB types

There are several datatypes provided by ScalaBP (or Google PB) which are not automatically converted into Scala's types,
that Chimney could convert for you:

  - `com.google.protobuf.empty.Empty` into `scala.Unit`
  - anything into `com.google.protobuf.empty.Empty`
  - `com.google.protobuf.duration.Duration` from/into `java.time.Duration`/`java.time.FiniteDuration`
  - `com.google.protobuf.timestamp.Timestamp` from/to `java.time.Instant`
  - `com.google.protobuf.ByteString` from/to any Scala collections of `Byte`s
  - wrapping/unwrapping Scala primitives with/from: 
    - `com.google.protobuf.wrappers.BoolValue` (`Boolean`)
    - `com.google.protobuf.wrappers.BytesValue` (collection of `Byte`s)
    - `com.google.protobuf.wrappers.DoubleValue` (`Double`)
    - `com.google.protobuf.wrappers.Int32Value` (`Int`)
    - `com.google.protobuf.wrappers.Int64Value` (`Long`)
    - `com.google.protobuf.wrappers.UInt32Value` (`Int`)
    - `com.google.protobuf.wrappers.UInt64Value` (`Long`)
    - `com.google.protobuf.wrappers.StringValue` (`String`)

Each of these transformations is provided by the same import:

!!! example

    ```scala
    //> using dep io.scalaland::chimney-protobufs::{{ chimney_version() }}
    import io.scalaland.chimney.protobufs._
    ```

## Libraries with smart constructors

Any type that uses a smart constructor (returning parsed result rather than throwing an exception) would require
Partial Transformer rather than Total Transformer to convert.

If there is no common interface that could be summoned as implicit for performing smart construction:

!!! example

    Assuming Scala 3 or `-Xsource:3` for fixed `private` constructors so that `Username.apply` and `.copy` would
    be private. (Newest versions of Scala 2.13 additionally require us to acknowledge this change in the behavior by
    manually suppressing an error/warning).

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Xsource:3 -Wconf:cat=scala3-migration:s
    final case class Username private (value: String)
    object Username {
      def parse(value: String): Either[String, Username] =
        if (value.isEmpty) Left("Username cannot be empty")
        else Right(Username(value))
    }
    ```

    ```scala
    //> using scala {{ scala.3 }}
    final case class Username private (value: String)
    object Username {
      def parse(value: String): Either[String, Username] =
        if value.isEmpty then Left("Username cannot be empty")
        else Right(Username(value))
    }
    ```

then Partial Transformer would have to be created manually:

!!! example

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Xsource:3 -Wconf:cat=scala3-migration:s
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    
    final case class Username private (value: String)
    object Username {
      def parse(value: String): Either[String, Username] =
        if (value.isEmpty) Left("Username cannot be empty")
        else Right(Username(value))
    }
    
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.partial

    implicit val usernameParse: PartialTransformer[String, Username] =
      PartialTransformer[String, Username] { value =>
        partial.Result.fromEitherString(Username.parse(value))
      }
    ```

However, if there was some type class interface, e.g.

!!! example

    ```scala
    trait SmartConstructor[From, To] {
      def parse(from: From): Either[String, To]
    }
    ```

!!! example

    ```scala
    object Username extends SmartConstructor[String, Username] {
      // ...
    }
    ```

we could use it to construct `PartialTransformer` automatically:

!!! example

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Xsource:3 -Wconf:cat=scala3-migration:s
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.partial

    trait SmartConstructor[From, To] {
      def parse(from: From): Either[String, To]
    }

    implicit def smartConstructedPartial[From, To](implicit
        smartConstructor: SmartConstructor[From, To]
    ): PartialTransformer[From, To] =
      PartialTransformer[From, To] { value =>
        partial.Result.fromEitherString(smartConstructor.parse(value))
      }
      
    final case class Username private (value: String)
    object Username extends SmartConstructor[String, Username] {
      def parse(value: String): Either[String, Username] =
        if (value.isEmpty) Left("Username cannot be empty")
        else Right(Username(value))
    }
    ```

The same would be true about extracting values from smart-constructed types
(if they are not ``AnyVal``\s, handled by Chimney out of the box).

Let's see how we could implement support for automatic transformations of
types provided in some popular libraries.

### Scala NewType

[NewType](https://github.com/estatico/scala-newtype) is a macro-annotation-based
library which attempts to remove runtime overhead from user's types.

!!! example

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Ymacro-annotations
    //> using dep io.estatico::newtype::0.4.4
    import io.estatico.newtype.macros.newtype

    @newtype case class Username(value: String)
    ```

would be rewritten to become `String` in the runtime, while prevent
mixing `Username` values with other `String`s accidentally.

NewType provides `Coercible` type
class [to allow generic wrapping and unwrapping](https://github.com/estatico/scala-newtype#coercible-instance-trick)
of `@newtype` values. This type class is not able to validate the cast type, so it is safe to use only if NewType is used
as a wrapper around another type that performs this validation e.g. Refined Type.

!!! example

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Ymacro-annotations
    //> using dep io.estatico::newtype::0.4.4
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.estatico.newtype.Coercible
    import io.scalaland.chimney.Transformer

    implicit def newTypeTransformer[From, To](implicit
        coercible: Coercible[From, To]
    ): Transformer[From, To] = coercible(_)
    ```

### Monix Newtypes

[Monix's Newtypes](https://newtypes.monix.io/) is similar to NewType in that
it tries to remove wrapping in runtime. However, it uses different tricks
(and syntax) to achieve it.

!!! example

    ```scala
    //> using dep io.monix::newtypes-core::0.2.3
    import monix.newtypes._

    type Username = Username.Type
    object Username extends NewtypeValidated[String] {
      def apply(value: String): Either[BuildFailure[Type], Type] =
        if (value.isEmpty) Left(BuildFailure("Username cannot be empty"))
        else Right(unsafeCoerce(value))
    }
    ```

Additionally, it provides 2 type classes: one to extract value
(`HasExtractor`) and one to wrap it (possibly validating, `HasBuilder`).
We can use them to provide unwrapping `Transformer` and wrapping
`PartialTransformer`:

!!! example

    ```scala
    //> using dep io.monix::newtypes-core::0.2.3
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.{PartialTransformer, Transformer}
    import io.scalaland.chimney.partial
    import monix.newtypes._

    implicit def unwrapNewType[Outer, Inner](implicit
        extractor: HasExtractor.Aux[Outer, Inner]
    ): Transformer[Outer, Inner] = extractor.extract(_)

    implicit def wrapNewType[Inner, Outer](implicit
        builder: HasBuilder.Aux[Outer, Inner]
    ): PartialTransformer[Inner, Outer] = PartialTransformer[Inner, Outer] { value =>
      partial.Result.fromEitherString(
        builder.build(value).left.map(_.toReadableString)
      )
    }
    ```

### Refined Types

[Refined Types](https://github.com/fthomas/refined) is a library aiming to provide automatic validation of some
popular constraints as long as we express them in the value's type.

!!! example

    ```scala
    //> using dep eu.timepit::refined::0.11.1
    import eu.timepit.refined._
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.auto._
    import eu.timepit.refined.collection._

    type Username = String Refined NonEmpty
    ```

We can validate using the dedicated type class (`Validate`), while extraction is a simple accessor:

!!! example

    ```scala
    //> using dep eu.timepit::refined::0.11.1
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import eu.timepit.refined.refineV
    import eu.timepit.refined.api.{Refined, Validate}
    import io.scalaland.chimney.{PartialTransformer, Transformer}
    import io.scalaland.chimney.partial

    implicit def extractRefined[Type, Refinement]: Transformer[Type Refined Refinement, Type] =
      _.value

    implicit def validateRefined[Type, Refinement](implicit
      validate: Validate.Plain[Type, Refinement]
    ): PartialTransformer[Type, Type Refined Refinement] =
      PartialTransformer[Type, Type Refined Refinement] { value =>
        partial.Result.fromEitherString(refineV[Refinement](value))
      }
    ```

## Custom default values

If you are providing integration for a type which you do not control, and you'd like to let your users fall back
to default values when using Chimney, but the type does not define them - it might be still possible to provide them
with `io.scalaland.chimney.integrations.DefaultValue`. It could look like this:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    // Types which we cannot simply edit: come from external library, codegen, etc.
    
    case class MyType(int: Int)
    case class Foo(a: Int)
    case class Bar(a: Int, b: MyType)
    
    // Our integration:
    implicit val defaultMyType: io.scalaland.chimney.integrations.DefaultValue[MyType] = () => MyType(0)
    
    // Remember that default values has to be enabled!
    import io.scalaland.chimney.dsl._
    pprint.pprintln(
      Foo(10).into[Bar].enableDefaultValues.transform
    )
    pprint.pprintln(
      Foo(10).into[Bar].enableDefaultValueOfType[MyType].transform
    )
    // expected outputs:
    // Bar(a = 10, b = MyType(int = 0))
    // Bar(a = 10, b = MyType(int = 0))
    ``` 

Keep in mind, that such provision works for every constructor which has an argument of such type not matched with source
value, so it's only safe to use when in the scope which sees such implicit all derivations would only need default value
of this type, rather than convert it from something else.

## Custom optional types

In case your library/domain defines custom Optional types, you can provide your own handling of such types through
`io.scalaland.chimney.integrations.OptionalValue`. It could look like this:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    // When you define your own optional...
    sealed trait MyOptional[+A]
    object MyOptional {
      case class Present[+A](value: A) extends MyOptional[A]
      case object Absent extends MyOptional[Nothing]
      
      def apply[A](value: A): MyOptional[A] = if (value != null) Present(value) else Absent
    }
    
    // ...you can provide Chimney support for it...
    import io.scalaland.chimney.integrations.OptionalValue

    implicit def myOptionalIsOptionalValue[A]: OptionalValue[MyOptional[A], A] = new OptionalValue[MyOptional[A], A] {
      override def empty: MyOptional[A] = MyOptional.Absent
      // to match Option's' behavior, it should handle nulls
      override def of(value: A): MyOptional[A] = MyOptional(value)
      override def fold[A0](oa: MyOptional[A], onNone: => A0, onSome: A => A0): A0 = oa match {
        case MyOptional.Present(value) => onSome(value)
        case MyOptional.Absent         => onNone
      }
    }
    
    // ...so you could use it:
    import io.scalaland.chimney.dsl._
    
    // for converting between Option and custom optional type
    pprint.pprintln(
      Option("test").transformInto[MyOptional[String]]
    )
    pprint.pprintln(
      MyOptional("test").transformInto[Option[String]]
    )
    // expected output:
    // Present(value = "test")
    // Some(value = "test")
    
    // for automatinc wrapping with custom optional type
    pprint.pprintln(
      "test".transformInto[MyOptional[String]]
    )
    // expected output:
    // Present(value = "test")
    
    // for safe unwrapping with PartialTransformers
    pprint.pprintln(
      MyOptional("test").transformIntoPartial[String].asOption
    )
    pprint.pprintln(
      MyOptional("test").transformIntoPartial[String].asOption
    )
    // expected output:
    // Some(value = "test")
    // Some(value = "test")
    
    case class Foo(value: String)
    case class Bar(value: String, another: Double)
    
    // for overriding values with path to optional value like with Option
    pprint.pprintln(
      MyOptional(Foo("test"))
        .into[MyOptional[Bar]]
        .withFieldConst(_.matchingSome.another, 3.14)
        .transform
    )
    // expected output:
    // Present(value = Bar(value = "test", another = 3.14))
    ```

As you can see, once you provide 1 implicit your custom optional type:

 * can be converted to/from `scala.Option` (and other optional types)
 * can automatically wrap value
 * can automatically unwrap value in `PartialTransformer`s
 * can be used with `matchingSome` path in `withFieldConst`/`withFieldComputed`/etc

An example of such optional type is `java.util.Optional` for which support is provided via `OptionalValue` in
[Java collections' integration](#java-collections-integration).

## Custom collection types

In case your library/domain defines custom collections - which are:
 * NOT providing `scala.collection.Factory` (2.13/3) or `scala.collection.genric.CanBuildFrom` (2.12)
 * or NOT extending `Iterable`

you have to provide some configuration to help Chimney work with them.

Most of the time a collection doesn't perform any sort of validations, and you can always put items in it:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    // When you define your own collection...
    class MyCollection[+A] private (private val impl: Vector[A]) {
  
      def iterator: Iterator[A] = impl.iterator
  
      override def equals(obj: Any): Boolean = obj match {
        case myCollection: MyCollection[?] => impl == myCollection.impl
        case _                             => false
      }
      override def hashCode(): Int = impl.hashCode()
      override def toString: String = impl.mkString("MyCollection(", ", ", ")")
    }
    object MyCollection {
  
      def of[A](as: A*): MyCollection[A] = new MyCollection(as.toVector)
      def from[A](vector: Vector[A]): MyCollection[A] = new MyCollection(vector)
    }
    
    // ...you can provide Chimney support for it...
    import io.scalaland.chimney.integrations.{ FactoryCompat, TotallyBuildIterable }
    import scala.collection.compat._
    import scala.collection.mutable

    implicit def myCollectionIsTotallyBuildIterable[A]: TotallyBuildIterable[MyCollection[A], A] =
      new TotallyBuildIterable[MyCollection[A], A] {
        // Factory for your type
        def totalFactory: Factory[A, MyCollection[A]] = new FactoryCompat[A, MyCollection[A]] {
  
          override def newBuilder: mutable.Builder[A, MyCollection[A]] =
            new FactoryCompat.Builder[A, MyCollection[A]] {
              private val implBuilder = Vector.newBuilder[A]
  
              override def clear(): Unit = implBuilder.clear()
  
              override def result(): MyCollection[A] = MyCollection.from(implBuilder.result())
  
              override def addOne(elem: A): this.type = { implBuilder += elem; this }
            }
        }
  
        // your type as Iterator
        override def iterator(collection: MyCollection[A]): Iterator[A] = collection.iterator
      }
      
    // ...so you could use it:
    import io.scalaland.chimney.dsl._
    
    // for converting to and from standard library collection (or any other type supported this way)
    pprint.pprintln(
      MyCollection.of("a", "b").transformInto[List[String]]
    )
    pprint.pprintln(
      List("a", "b").transformInto[MyCollection[String]]
    )
    // expected output:
    // List("a", "b")
    // MyCollection(a, b)
    
    case class Foo(value: String)
    case class Bar(value: String, another: Double)
    
    // for overriding values with path to items like with standard library's collections
    pprint.pprintln(
      List(Foo("test"))
        .into[MyCollection[Bar]]
        .withFieldConst(_.everyItem.another, 3.14)
        .transform
    )
    // expected output:
    // MyCollection(Bar(test,3.14))
    ```

If your collection performs some sort of validation, you integrate it with Chimney as well:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    // When you define your own collection...
    class NonEmptyCollection[+A] private (private val impl: Vector[A]) {
    
      def iterator: Iterator[A] = impl.iterator
  
      override def equals(obj: Any): Boolean = obj match {
        case nonEmptyCollection: NonEmptyCollection[?] => impl == nonEmptyCollection.impl
        case _                                         => false
      }
      override def hashCode(): Int = impl.hashCode()
      override def toString: String = impl.mkString("NonEmptyCollection(", ", ", ")")
    }
    object NonEmptyCollection {
  
      def of[A](a: A, as: A*): NonEmptyCollection[A] = new NonEmptyCollection(a +: as.toVector)
      def from[A](vector: Vector[A]): Option[NonEmptyCollection[A]] =
        if (vector.nonEmpty) Some(new NonEmptyCollection(vector)) else None
    }
    
    // ...you can provide Chimney support for it...
    import io.scalaland.chimney.integrations.{ FactoryCompat, PartiallyBuildIterable }
    import io.scalaland.chimney.partial
    import scala.collection.compat._
    import scala.collection.mutable

    implicit def nonEmptyCollectionIsPartiallyBuildIterable[A]: PartiallyBuildIterable[NonEmptyCollection[A], A] =
      new PartiallyBuildIterable[NonEmptyCollection[A], A] {
  
        // notice, that this Factory returns partial.Result of your collection!
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
      
    // ...so you could use it:
    import io.scalaland.chimney.dsl._
    
    // for validating that your collection can be created once all items have been put into Builder
    pprint.pprintln(
      List("a").transformIntoPartial[NonEmptyCollection[String]].asOption
    )
    pprint.pprintln(
      List.empty[String].transformIntoPartial[NonEmptyCollection[String]].asOption
    )
    // expected output:
    // Some(value = NonEmptyCollection(a))
    // None
    ```
    
For map types there are specialized versions of these type classes:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    
    import io.scalaland.chimney.integrations._
    import io.scalaland.chimney.partial
    import scala.collection.compat._
    import scala.collection.mutable

    class MyMap[+K, +V] private (private val impl: Vector[(K, V)]) {
  
      def iterator: Iterator[(K, V)] = impl.iterator
  
      override def equals(obj: Any): Boolean = obj match {
        case customMap: MyMap[?, ?] => impl == customMap.impl
        case _                          => false
      }
      override def hashCode(): Int = impl.hashCode()
    }
    object MyMap {
  
      def of[K, V](pairs: (K, V)*): MyMap[K, V] = new MyMap(pairs.toVector)
      def from[K, V](vector: Vector[(K, V)]): MyMap[K, V] = new MyMap(vector)
    }
  
    implicit def customMapIsTotallyBuildMap[K, V]: TotallyBuildMap[MyMap[K, V], K, V] =
      new TotallyBuildMap[MyMap[K, V], K, V] {
  
        def totalFactory: Factory[(K, V), MyMap[K, V]] = new FactoryCompat[(K, V), MyMap[K, V]] {
  
          override def newBuilder: mutable.Builder[(K, V), MyMap[K, V]] =
            new FactoryCompat.Builder[(K, V), MyMap[K, V]] {
              private val implBuilder = Vector.newBuilder[(K, V)]
  
              override def clear(): Unit = implBuilder.clear()
  
              override def result(): MyMap[K, V] = MyMap.from(implBuilder.result())
  
              override def addOne(elem: (K, V)): this.type = { implBuilder += elem; this }
            }
        }
  
        override def iterator(collection: MyMap[K, V]): Iterator[(K, V)] = collection.iterator
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
  
      def of[K, V](pair: (K, V), pairs: (K, V)*): NonEmptyMap[K, V] = new NonEmptyMap(pair +: pairs.toVector)
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
    ```
    
The only 2 difference they make is that:
 - when we are converting with `PartialTransformer` failures will be reported on map keys instead of `_1` and `_2` field
   of a tuple in a sequence (e.g. `keys(myKey)` - if key conversion failed for `myKey` value or `(myKey)` if value
   conversion failed for `myKey` key, instead of `(0)._1` or `(0)._2`)
 - they allow usage of `everyMapKey` and `everyMapValue` in paths, just like with standard library's `Maps`.

An example of such collections are `java.util` collections for which support is provided via `TotallyBuildIterable` 
and `TotallyBuildMap` in [Java collections' integration](#java-collections-integration), or `cats.data` types
provided in [Cats integration](#cats-integration).
