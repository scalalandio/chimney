# Troubleshooting

Already using Chimney and you've got some issues? This page might help you with it.

## Goals and non-goals

While Chimney is usually used to convert one piece of immutable data into another piece of immutable data, not every
time you are working with immutable values you need Chimney.

The main goal of Chimney is minimizing the amount of code you have to write to convert a value of one type (known at
compile time) into a value of another type (also known at compile time). While there are Patchers, their goal is to
update one value, using another value and nothing more.

If you:

  - receive some unstructured, raw input value (String, InputStream, binary data, ...) you are not looking for Chimney
    when it comes to parsing it - you need a parser library. Maybe it will be a JSON parser, or something you could
    build with parser combinators, or a code generated with Interface Description Language (IDL) like Protocol Buffers,
    Swagger or AsyncAPI.
    - However, if you parse raw data into some structured data, that structured data can be used by Chimney to convert
      into e.g. domain model
  - want to update immutable data by passing some path to the updated field and then provide a value - you MAY need
    a lens library like Quicklens or Monocle, although Chimney has
    [a limited support some lens use case](cookbook.md#lens-like-use-cases)
  - want to limit the amount of tests written and are wondering if automatic generation of such an important code is
    safe - you need to ask yourself: would you have the same dilemma if you were asked about generating JSON codecs?
    Would you wonder if you need to test them? (In our experience, yes). Could you remove the need to test them if you
    mandated the code to be always/never generated? (In our experience, whether its generated or written by hand you
    want to test it).
    - You can however avoid testing someone else's library if you use Chimney in place, in some service, and then test
      that service's behavior or if you create in your DTO model `def toDomain = this.transformInto[DomainModel]`.
      You can utilize code generation without making your application's type signatures depend on someone else's types. 

## Migration from 0.8.x to 1.0.0

As long as you did not:

 - use values stored in `internal` packages
 - selectively import implicits
 - use JavaFactories and JavaIterables (in chimney-java-collections package)

you can assume that most changes are source compatible. The only explicit changes to API are:

 - deprecation of `withCoproductInstance` - method is still available but deprecated in favor of `withEnumCaseHandled`
   and `withSealedSubtypeHandled` 
 - setters behavior was restored to how it used to work on 0.7.x - unary method not only has to start its name with
   `set` but also return `Unit`. If needed non-`Unit` setters can be enabled by an opt-in flag
 - the way of turning `Option`/`Either[String, *]`/`Try`/`cats.data.Validated` was unified - `AsResult[F]` type class
   was introduced, and  `import io.scalaland.chimney.partial.syntax.*` provides `fa.asResult` syntax. So
   `validated.toPartialResult` has to be rewritten into `validated.asResult`.

While most changes are source backward compatible, a lot of internals had to be revamped to fix bugs and unblock
further development. For that reason these changes are not binary backward compatible, and have to be considered 
breaking changes when it comes to linking with previously compiled code. However, this cleanup should eliminate the need
for any such refactors for a long time.

### Deprecation of `withCoproductInstance`

Method was marked as `@deprecated` but NOT removed, so you can keep using it (as long as you are not using
`-Xfatal-warnings`). It ir recommended though to rename each such usage into either `withEnumCaseHandled` or
`withSealedSubtypeHandled` simple for the sake of readability.

### Handling non-`Unit` method with names beginning with `set`

Prior to 0.8.0 Chimney assumed that setters:

 * has to begin their name with `set`
 * has to be unary method (1 value parameter, no type parameters, single argument list)
 * has to return `Unit`

Chimney 0.8.0 relaxed the last condition, to allow targeting setters in builders which might have `set` methods, that
mutate but return e.g. `this.type`. However, it broke the code for people using `set` as e.g. methods concerning
mathematical sets.

Since 1.0.0 Chimney makes non-`Unit` setters opt-in - it still allows to use them but requires enabling them
[with a flag](supported-transformations.md#writing-to-non-unit-bean-setters).

### Migrating to `asResult`

Chimney 0.8.5 introduced `AsResult` type class:

!!! example

    ```scala
    trait AsResult[F[_]] {
      def asResult[A](fa: F[A]): Result[A]
    }
    ```

which can be used together with `io.scalaland.chimney.partial.syntax._` to provide `asResult` extension:

!!! example

    ```scala
    import io.scalaland.chimney.partial.syntax._
    
    (Left("error"): Either[String, Int]).asResult // partial.Result[Int]
    ```

Meanwhile, Chimney Cats' module used to define `toPartialResult` extension methods to handle conversion from
`cats.Validated` into `partial.Result`. Now, it uses `asResult` as well - to migrate you simple have to replace all
occurrences of `toPartialResult` with `asResult`. 

## Migration from 0.7.x to 0.8.0

Version 0.8.0 is the first version that cleaned up the API. It introduced several breaking changes.

### Replacing Lifted Transformers (`TransformerF`) with `PartialTransformer`s

Lifted Transformers (`TransformerT`), deprecated in 0.7.0, got removed in favor of `PartialTransformer`s

Chimney's Lifted Transformers were historically the first experimental attempt
to express transformations that may potentially fail. Despite their great expressiveness, they lacked several basic
features and had a few design flaws that make them unattractive/difficult for wider adoption.

Let's have a look at the type signatures of both Lifted and Partial Transformers.

!!! example

    ```scala
    package io.scalaland.chimney

    // Lifted Transformer
    trait TransformerF[F[+_], From, To] {
      def transform(src: From): F[To]
    }

    // Partial Transformer
    // partial comes from io.scalaland.chimney.partial
    trait PartialTransformer[From, To] {
      def transform(src: From, failFast: Boolean): partial.Result[To]
    }
    ```

  - Lifted Transformers provided abstraction over the target transformation type container (``F[+_]``), while
    Partial Transformers fix resulting type to built-in ``partial.Result[_]``
    - as a consequence of this abstraction, Lifted Transformers required a type class instance
      (`TransformerFSupport`) in scope for every specific `F[_+]` used
    - Partial Transformers rely on built-in behavior and provide convenience methods to convert between more familiar
      data types (`Option`, `Either`, etc.)
    - abstraction over the resulting container type in the Lifted Transformer allowed for having custom error types;
      this is not easily possible with Partial Transformer, which focuses on a few most common error types
  - Partial Transformer has built-in support for fail-fast (short-circuiting) semantics by passing `failFast`
    boolean parameter, while in Lifted Transformers it was barely possible (only by providing a supporting type class
    that had such a fixed behavior)
  - Error path support in Lifted Transformers required providing another type class instance
    (`TransformerFErrorPathSupport`) for your error collection type,
    while in partial transformers it is a built-in feature

To migrate your code from Lifted Transformers to Partial Transformers, you may take the following steps.

  - replace all the occurrences of `TransformerF` type with `PartialTransformer` and remove the first type argument
    (`F[_]`) which is not used for Partial Transformers.
  - for your transformations find corresponding DSL methods. Their name usually differs on the suffix, for example:
    - replace ``withFieldConstF`` with ``withFieldConstPartial``
    - replace ``withFieldComputedF`` with ``withFieldComputedPartial``
    - etc.
  - adjust the types passed to the customization methods. In Lifted Transformers, they were expecting values of
    your custom type `F[A]`, while in Partial Transformers they work with `partial.Result[A]`. See the
    `partial.Result` companion object for ways of constructing `success` and `failure` instances, for example:
    - `partial.Result.fromValue`
    - `partial.Result.fromOption`
    - `partial.Result.fromEither`
    - `partial.Result.fromTry`
    - and so on...
  - the resulting type of the call to `.transform` is also a `partial.Result[A]`. If you don't want to work with
    `partial.Result` directly, figure out ways to convert it to other, more familiar data structures.
    Some of the ways may include:
    - `result.asOption`
    - `result.asEither`
    - `result.asErrorPathMessages`
    - other data structures using [Cats' integration](cookbook.md#cats-integration)

### Removal of `.enableUnsafeOption`

`.enableUnsafeOption`was removed - if `Option` unwrapping is needed, it is recommended to use `PartialTransformer`

This option allowed calling `.get` on `Option` to enable conversion from `Option` to non-`Option:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::0.7.5
    import io.scalaland.chimney.dsl._

    case class Foo(a: Option[String])
    case class Bar(a: String)

    Foo(Some("value")).into[Bar].enableUnsafeOption.transform // Bar("value")
    try {
      Foo(None).into[Bar].enableUnsafeOption.transform // throws Exception
    } catch {
      case e: Throwable => println(e)
    }
    ```

Throwing exceptions made sense as a workaround in simpler times, when `Transformer`s were the only option. However,
now we have `PartialTransformer`s. They have a build-in ability to unwrap `Option` as failed result.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: Option[String])
    case class Bar(a: String)

    pprint.pprintln(
      Foo(Some("value")).transformIntoPartial[Bar](failFast = true).asOption
    )
    pprint.pprintln(
      Foo(None).transformIntoPartial[Bar](failFast = true).asOption
    )
    // expected output:
    // Some(value = Bar(a = "value"))
    // None
    ```

With `failFast = true` and `.asOption` Partial Transformers have similar semantics to `Transformer` with unsafe `Option`
but the result is an explicit `Option` instead of implied exception handling. 

### Changes to automatic derivation logic

Types returned by automatic derivation got split from types that are used
for user-provided transformations and configured (semiautomatic) derivation:
`Transformer` got split into `Transformer` and `Transformer.AutoDerived`
while `PartialTransformer` got split into `PartialTransformer` and
`PartialTransformer.AutoDerived`.

It was caused by the change in the mechanism for recursive derivation: since Chimney
avoid boxing and allocation where possible, it is used to check if summoned
implicit was generated by automatic derivation. If implicit came from
`Transformer.derive` or `PartialTransformer.derive` it was discarded and
the macro attempted to derive it again without wrapping the result in a type class.

It both complicated code and increased compilation times, as each field
or subtype would attempt to summon implicit (potentially triggering macro expansion)
and then discard it if it didn't come from the user. Splitting types allows the compiler
to not summon any implicit if the user hasn't provided any.

The consequence is only visible if there is some `implicit def` which takes
another implicit `Transformer`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.Transformer

    class MyType[A](private val a: A) {
      def map[B](f: A => B): MyType[B] =
        new MyType(f(a))
    }

    implicit def provideMyType[A, B](implicit
        a2b: Transformer[A, B]
    ): Transformer[MyType[A], MyType[B]] =
      myA => myA.map(_.transformInto[B])
    ```

After changes in 0.8.x `implicit Transformer[A, B]` means "instance provided by user",
either manually or through semiautomatic derivation. If the user wants to allow summoning
there the automatic instances as well, they need to use `Transformer.AutoDerived`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.Transformer

    class MyOtherType[A](private val a: A) {
      def map[B](f: A => B): MyOtherType[B] =
        new MyOtherType(f(a))
    }

    implicit def provideMyOtherType[A, B](implicit
        a2b: Transformer.AutoDerived[A, B]
    ): Transformer[MyOtherType[A], MyOtherType[B]] =
      myA => myA.map(_.transformInto[B])
    ```

which would summon both automatically derived instances and manually provided ones.
The difference is shown in this example:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.Transformer

    class MyType[A](private val a: A) {
      def map[B](f: A => B): MyType[B] =
        new MyType(f(a))
      override def toString: String = s"MyType($a)"
    }
    
    implicit def provideMyType[A, B](implicit
        a2b: Transformer[A, B]
    ): Transformer[MyType[A], MyType[B]] =
      myA => myA.map(_.transformInto[B])
    
    class MyOtherType[A](private val a: A) {
      def map[B](f: A => B): MyOtherType[B] =
        new MyOtherType(f(a))
      override def toString: String = s"MyOtherType($a)"
    }
    
    implicit def provideMyOtherType[A, B](implicit
        a2b: Transformer.AutoDerived[A, B]
    ): Transformer[MyOtherType[A], MyOtherType[B]] =
      myA => myA.map(_.transformInto[B])

    // implicit provided by the user
    implicit val int2str: Transformer[Int, String] = _.toString

    val myType: MyType[Int] = new MyType(10)
    val myOtherType: MyOtherType[Int] = new MyOtherType(10)

    // uses provideMyType(int2str):
    pprint.pprintln(
      myType.transformInto[MyType[String]]
    )
    // expected output:
    // MyType(10)

    // uses provideMyOtherType(int2str):
    pprint.pprintln(
      myOtherType.transformInto[MyOtherType[String]]
    )
    // expected output:
    // MyOtherType(10)

    val myType2: MyType[Either[Int, Int]] = new MyType(Right(10))
    val myOtherType2: MyOtherType[Either[Int, Int]] = new MyOtherType(Right(10))

    // requires manually provided transformer e.g.
    //   implicit val either2either =
    //     Transformer.derive[Either[Int, Int], Either[String, String]]
    // without it, the compilation fails
    // myType2.transformInto[MyType[Either[String, String]]]

    // uses provideMyOtherType(Transformer.derive):
    pprint.pprintln(
      myOtherType2.transformInto[MyOtherType[Either[String, String]]]
    )
    // expected output:
    // MyOtherType(Right(10))
    ```

### Default values no longer are used as fallback if the source field exists

If:

  - default values were enabled,
  - source and target had fields of the same name
  - this field had default value defined
  - macro couldn't derive transformation from source field type to target field type

Chimney used to use the default value.

However, this was a buggy behavior, and currently, it only uses default values
if there is no source field nor other fallback or override. Although it is
a bugfix, it is also a breaking change so it has to be documented. The fix would
be a manual resolution for all fields which now (correctly) fail due to the bugfix.

## Migration from 0.6.x to 0.7.0

The only change in the behavior that might require manual action was making default values opt-in for safety concerns.
Now, user has to manually enable them. 

### Explicit enabling of default values

Default values were already controllable with a flag, so
[the API remains the same](supported-transformations.md#allowing-fallback-to-the-constructors-default-values).
Only the initial value of the`DefaultValues` flag changed.

If you used default values a lot, remember that you can enable them for all transformation with a scope with:

!!! example

    ```scala
    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableDefaultValues
    ```

## Coming from other type-mapping libraries

Chimney is not the first type-mapping library, and it doesn't have a monopoly over various solutions. The best known are
probably C#'s AutoMapper (the first release on May 2010) and Java's MapStruct (the first release June 2013).
(For the record, the first Chimney's release was on May 2017, so it's younger than the first 2 Scala libraries described
below).

You might have come here as a user of another solution, and you might be curious how your current use cases translates
to Chimney, and what are the differences between the libraries.

This section is dedicated to making it easier to migrate or to understand the differences between other solutions
and Chimney.

### Scala Automapper

!!! warning

    The comparison was made against the version `{{ libraries.scala_automapper }}`.
    If it's out-of-date, please let us know, or even better, provide a PR with an update!
    
[Scala Automapper](https://github.com/bfil/scala-automapper) was first released in September 2015. Its latest version,
similarly to Chimney, is based on macros. It only supports Scala 2.13 and only on JVM. Previous release, `0.6.2`, was
released for Scala 2.12 and 2.11.

Here are some features it shares with Chimney (Automapper's code based on examples in its README):

!!! example "The simplest in-place mapping"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.bfil::automapper::{{ libraries.scala_automapper }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class SourceClass(label: String, value: Int)
    case class TargetClass(label: String, value: Int)
    
    import io.bfil.automapper._
    
    val source = SourceClass("label", 10)
    val target = automap(source).to[TargetClass]
    
    pprint.pprintln(target)
    // expected output:
    // TargetClass(label = "label", value = 10)
    ```
    
    Chimney's counterpart:
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    case class SourceClass(label: String, value: Int)
    case class TargetClass(label: String, value: Int)
    
    import io.scalaland.chimney.dsl._
    
    val source = SourceClass("label", 10)
    val target = source.transformInto[TargetClass]
    
    pprint.pprintln(target)
    // expected output:
    // TargetClass(label = "label", value = 10)
    ```

!!! example "Defining transformation in one place as implicit"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.bfil::automapper::{{ libraries.scala_automapper }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    case class SourceClass(label: String, value: Int)
    case class TargetClass(label: String, value: Int)
    case class AnotherClass(label: String, value: Int)
    
    import io.bfil.automapper._
    
    val source = SourceClass("label", 10)
    
    trait MyMappings {
      implicit val mapping1 = generateMapping[SourceClass, TargetClass]
      implicit val mapping2 = generateMapping[SourceClass, AnotherClass]
    }
    
    object Example extends MyMappings {
      val target1 = automap(source).to[TargetClass]
      val target2 = automap(source).to[AnotherClass] 
    }
    
    pprint.pprintln(Example.target1)
    pprint.pprintln(Example.target2)
    // expected output:
    // TargetClass(label = "label", value = 10)
    // AnotherClass(label = "label", value = 10)
    ```
    
    Chimney's counterpart:
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    case class SourceClass(label: String, value: Int)
    case class TargetClass(label: String, value: Int)
    case class AnotherClass(label: String, value: Int)
    
    val source = SourceClass("label", 10)

    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._
    
    trait MyMappings {
      implicit val mapping1 = Transformer.derive[SourceClass, TargetClass]
      implicit val mapping2 = Transformer.derive[SourceClass, AnotherClass]
    }
    
    object Example extends MyMappings {
      val target1 = source.transformInto[TargetClass]
      val target2 = source.transformInto[AnotherClass]
    }
    
    pprint.pprintln(Example.target1)
    pprint.pprintln(Example.target2)
    // expected output:
    // TargetClass(label = "label", value = 10)
    // AnotherClass(label = "label", value = 10)
    ```
    
!!! example "Automapper's dynamic mappings"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.bfil::automapper::{{ libraries.scala_automapper }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    case class SourceClass(label: String, field: String, list: List[Int])
    case class TargetClass(label: String, renamedField: String, total: Int)
    
    import io.bfil.automapper._
    
    val source = SourceClass("label", "field", List(1, 2, 3))
    
    val values = source.list // List(1, 2, 3)
    def sum(values: List[Int]) = values.sum
    
    val target = automap(source).dynamicallyTo[TargetClass](
      renamedField = source.field, total = sum(values)
    )
    pprint.pprintln(target)
    // expected output:
    // TargetClass(label = "label", renamedField = "field", total = 6)
    ```
    
    Depending on case, in Chimney we would call it rename, value provision, value computation.
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    case class SourceClass(label: String, field: String, list: List[Int])
    case class TargetClass(label: String, renamedField: String, total: Int)
    
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._
    
    val source = SourceClass("label", "field", List(1, 2, 3))
    
    val values = source.list // List(1, 2, 3)
    def sum(values: List[Int]) = values.sum
    
    val target = source.into[TargetClass]
      .withFieldRenamed(_.field, _.renamedField) // rename
      .withFieldConst(_.total, sum(values)) // value provision
      .transform
    // alternatively we don't need intermediate `values` and `sum`:
    val target2 = source.into[TargetClass]
      .withFieldRenamed(_.field, _.renamedField) // rename
      .withFieldComputed(_.total, src => src.list.sum) // value computation
      .transform
      
    pprint.pprintln(target)
    pprint.pprintln(target2)
    // expected output:
    // TargetClass(label = "label", renamedField = "field", total = 6)
    // TargetClass(label = "label", renamedField = "field", total = 6)
    ```

!!! example "Implicit conversion and polymorphic types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.bfil::automapper::{{ libraries.scala_automapper }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    trait SourceTrait
    case class SourceClassA(label: String, value: Int) extends SourceTrait
    case class SourceClassB(width: Int) extends SourceTrait
    
    trait TargetTrait
    case class TargetClassA(label: String, value: Int) extends TargetTrait
    case class TargetClassB(width: Int) extends TargetTrait
    
    case class SourceClass(field: SourceTrait)
    case class TargetClass(field: TargetTrait)
    
    import io.bfil.automapper._
    
    implicit def mapTrait(source: SourceTrait): TargetTrait = source match {
      case a: SourceClassA => automap(a).to[TargetClassA]
      case b: SourceClassB => automap(b).to[TargetClassB]
    }
    
    val source = SourceClass(SourceClassA("label", 10))
    val target = automap(source).to[TargetClass]
    pprint.pprintln(target)
    // expected output:
    // TargetClass(field = TargetClassA(label = "label", value = 10))
    ```
    
    In Chimney we are not relying on implicit conversions - instead we use implicit `Transformer`s when provided
    or derive the transformation recursively:
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    trait SourceTrait
    case class SourceClassA(label: String, value: Int) extends SourceTrait
    case class SourceClassB(width: Int) extends SourceTrait
    
    trait TargetTrait
    case class TargetClassA(label: String, value: Int) extends TargetTrait
    case class TargetClassB(width: Int) extends TargetTrait
    
    case class SourceClass(field: SourceTrait)
    case class TargetClass(field: TargetTrait)
    
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._
    
    implicit val sourceToTrait: Transformer[SourceTrait, TargetTrait] = {
      case a: SourceClassA => a.transformInto[TargetClassA]
      case b: SourceClassB => b.transformInto[TargetClassB]
    }

    val source = SourceClass(SourceClassA("label", 10))
    val target = source.transformInto[TargetClass]
    pprint.pprintln(target)
    // expected output:
    // TargetClass(field = TargetClassA(label = "label", value = 10))
    ```

Additionally, Scala Automapper supports:

 * automatically filling `Option` fields with `None` if there is no other source. For safety Chimney allows this as
   [opt-in feature](supported-transformations.md#allowing-fallback-to-none-as-the-constructors-argument)
 * automatically filling `Iterable`/`Map` is there is no other source. There is no direct correspondence in Chimney,
   as it requires providing such fields using [`withFieldConst`](supported-transformations.md#wiring-the-constructors-parameter-to-a-provided-value)
 * automatically filling default values if there is no other source. For safety Chimney allows this as
   [opt-in feature](supported-transformations.md#allowing-fallback-to-the-constructors-default-values)

On the other hand, Chimney additionally provides:

 * automatically mapping between [any class and any class with a public constructor](supported-transformations.md#into-a-case-class-or-pojo)
    * including [tuples](supported-transformations.md#frominto-a-tuple)
    * and allowing to provide/compute value into a nested field (`_.nested.field`) - including a field in `Option`
      (`_.matchingSome`), `Either` (`_.matchingLeft`, `_.matchingRight`), `Iterable` (`_.everyItem`) or `Map`
      (`_.everyMapKey`, `_.everyMapIndex`) 
 * automatically wrapping/unwrapping [`AnyVals`s](supported-transformations.md#frominto-an-anyval)
 * automatically mapping between [`sealed` types/Scala 3 `enum`s/Java `enum`s](supported-transformations.md#between-sealedenums)
 * automatically mapping between [collections](supported-transformations.md#between-scalas-collectionsarrays)
 * opt-in support for [reading from `def` methods](supported-transformations.md#reading-from-methods) and
   [inherited `val`s and `def`s](supported-transformations.md#reading-from-inherited-valuesmethods) 
 * opt-in support for Java Bean [getters](supported-transformations.md#reading-from-bean-getters) and
   [setters](supported-transformations.md#writing-to-bean-setters)
 * [`PartialTransformer`](supported-transformations.md#total-transformers-vs-partialtransformers), a type of
   transformation that might fail - think `Try`/`Either[String, _]`/`Option` all in one, with a full conversion or
   fail-fast as always available as a runtime flag
 * integrations to [Java's collections](cookbook.md#java-collections-integration), [Cats](cookbook.md#cats-integration),
   [Protocol Buffers](cookbook.md#protocol-buffers-integration) and your own [optional types](cookbook.md#custom-optional-types)
   and [collections](cookbook.md#custom-collection-types)

and more!

### Henkan

!!! warning

    The comparison was made against the version `{{ libraries.henkan }}`.
    If it's out-of-date, please let us know, or even better, provide a PR with an update!
    
[Henkan](https://github.com/kailuowang/henkan) was first released in March 2016. Its latest version, contrary to
Chimney, is based on [Shapeless](https://github.com/milessabin/shapeless/). It supports Scala 2.11, 2.12 and 2.13 on
JVM, Scala.js 0.6 (2.11, 2.12, 2.13), Scala.js 1.x (2.12, 2.13).

Here are some features it shares with Chimney (Henkan's code based on README):

!!! example "Transform between case classes"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kailuowang::henkan-convert::{{ libraries.henkan }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    import java.time.LocalDate
    
    case class Employee(name: String, address: String, dateOfBirth: LocalDate, salary: Double = 50000d)
    case class UnionMember(name: String, address: String, dateOfBirth: LocalDate)
    
    val employee = Employee("George", "123 E 86 St", LocalDate.of(1963, 3, 12), 54000)
    val unionMember = UnionMember("Micheal", "41 Dunwoody St", LocalDate.of(1994, 7, 29))
    
    import henkan.convert.Syntax._
    
    pprint.pprintln(
      employee.to[UnionMember]()
    )
    pprint.pprintln(
      unionMember.to[Employee]()
    )
    pprint.pprintln(
      unionMember.to[Employee].set(salary = 60000.0)
    )
    // expected output:
    // UnionMember(name = "George", address = "123 E 86 St", dateOfBirth = 1963-03-12)
    // Employee(name = "Micheal", address = "41 Dunwoody St", dateOfBirth = 1994-07-29, salary = 50000.0)
    // Employee(name = "Micheal", address = "41 Dunwoody St", dateOfBirth = 1994-07-29, salary = 60000.0)
    ```
    
    Chimney counterpart:
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    import java.time.LocalDate
    
    case class Employee(name: String, address: String, dateOfBirth: LocalDate, salary: Double = 50000d)
    case class UnionMember(name: String, address: String, dateOfBirth: LocalDate)
    
    val employee = Employee("George", "123 E 86 St", LocalDate.of(1963, 3, 12), 54000)
    val unionMember = UnionMember("Micheal", "41 Dunwoody St", LocalDate.of(1994, 7, 29))
    
    import io.scalaland.chimney.dsl._
    
    pprint.pprintln(
      employee.transformInto[UnionMember]
    )
    pprint.pprintln(
      unionMember.into[Employee].enableDefaultValues.transform
    )
    pprint.pprintln(
      unionMember.into[Employee].withFieldConst(_.salary, 60000.0).transform
    )
    // expected output:
    // UnionMember(name = "George", address = "123 E 86 St", dateOfBirth = 1963-03-12)
    // Employee(name = "Micheal", address = "41 Dunwoody St", dateOfBirth = 1994-07-29, salary = 50000.0)
    // Employee(name = "Micheal", address = "41 Dunwoody St", dateOfBirth = 1994-07-29, salary = 60000.0)
    ```

!!! example "Transform between case classes with optional field"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kailuowang::henkan-optional::{{ libraries.henkan }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    case class Message(a: Option[String], b: Option[Int])
    case class Domain(a: String, b: Int)
    
    import cats.data.Validated
    import cats.implicits._
    import henkan.optional.all._
    
    pprint.pprintln(
      validate(Message(Some("a"), Some(2))).to[Domain]
    )
    pprint.pprintln(
      validate(Message(Some("a"), None)).to[Domain]
    )
    // expected output:
    // Valid(a = Domain(a = "a", b = 2))
    // Invalid(e = NonEmptyList(head = RequiredFieldMissing(fieldName = "b"), tail = List()))
    
    pprint.pprintln(
      from(Domain("a", 2)).toOptional[Message]
    )
    // expected output:
    // Message(a = Some(value = "a"), b = Some(value = 2))
    ```
    
    For conversions that can fail (e.g. unwrapping `Option` value into non-`Option` field) Chimney provides deficated
    `PartialTransformer` which returns `partial.Result` - a type handling at once errors represented by:
    `scala.util.Try`, `scala.Option` and `scala.Either[String, ...]`. This result you can convert into whatever error
    type you want:
    
    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    case class Message(a: Option[String], b: Option[Int])
    case class Domain(a: String, b: Int)
    
    import io.scalaland.chimney.dsl._
    
    pprint.pprintln(
      Message(Some("a"), Some(2)).transformIntoPartial[Domain].asOption
    )
    pprint.pprintln(
      Message(Some("a"), None).transformIntoPartial[Domain].asOption
    )
    // expected output:
    // Some(value = Domain(a = "a", b = 2))
    // None
    
    import io.scalaland.chimney.cats._ // provides .asValidated
    
    pprint.pprintln(
      Message(Some("a"), Some(2)).transformIntoPartial[Domain].asValidated
    )
    pprint.pprintln(
      Message(Some("a"), None).transformIntoPartial[Domain].asValidated
    )
    // expected output:
    // Valid(a = Domain(a = "a", b = 2))
    // Invalid(
    //   e = Errors(
    //     errors = NonEmptyErrorsChain(
    //       Error(message = EmptyValue, path = Path(elements = List(Accessor(name = "b"))))
    //     )
    //   )
    // )
    ```

Chimney additionally provides:

 * defining whole conversion or its part through [implicits and `Transformer`/`PartialTransformer` type class](supported-transformations.md#custom-transformations)
 * automatically mapping between [any class and any class with a public constructor](supported-transformations.md#into-a-case-class-or-pojo)
    * including [tuples](supported-transformations.md#frominto-a-tuple)
    * and allowing to provide/compute value into a nested field (`_.nested.field`) - including a field in `Option`
      (`_.matchingSome`), `Either` (`_.matchingLeft`, `_.matchingRight`), `Iterable` (`_.everyItem`) or `Map`
      (`_.everyMapKey`, `_.everyMapIndex`) 
 * automatically wrapping/unwrapping [`AnyVals`s](supported-transformations.md#frominto-an-anyval)
 * automatically mapping between [`sealed` types/Scala 3 `enum`s/Java `enum`s](supported-transformations.md#between-sealedenums)
 * automatically mapping between [collections](supported-transformations.md#between-scalas-collectionsarrays)
 * opt-in support for [reading from `def` methods](supported-transformations.md#reading-from-methods) and
   [inherited `val`s and `def`s](supported-transformations.md#reading-from-inherited-valuesmethods) 
 * opt-in support for Java Bean [getters](supported-transformations.md#reading-from-bean-getters) and
   [setters](supported-transformations.md#writing-to-bean-setters)
 * [`PartialTransformer`](supported-transformations.md#total-transformers-vs-partialtransformers), a type of
   transformation that might fail - think `Try`/`Either[String, _]`/`Option` all in one, with a full conversion or
   fail-fast as always available as a runtime flag
 * integrations to [Java's collections](cookbook.md#java-collections-integration), [Cats](cookbook.md#cats-integration),
   [Protocol Buffers](cookbook.md#protocol-buffers-integration) and your own [optional types](cookbook.md#custom-optional-types)
   and [collections](cookbook.md#custom-collection-types)

and more!

### Ducktape

!!! warning

    The comparison was made against the version `{{ libraries.ducktape }}`.
    If it's out-of-date, please let us know, or even better, provide a PR with an update!
    
[Ducktape](https://arainko.github.io/ducktape/) was first released in November 2022. Its latest version, similarly to
Chimney, is based on macros. It supports only Scala 3 on JVM, Scala.js 1.x and Scala Native 0.5.

Here are some features it shares with Chimney (Ducktape's code based on GitHub Pages documentation):

!!! example "Using total transformations"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.github.arainko::ducktape::{{ libraries.ducktape }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    object wire:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: List[wire.PaymentMethod]
      )
    
      enum PaymentMethod:
        case Card(name: String, digits: Long)
        case PayPal(email: String)
        case Cash
    
    object domain:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: Vector[domain.PaymentMethod]
      )
    
      enum PaymentMethod:
        case PayPal(email: String)
        case Card(digits: Long, name: String)
        case Cash
    
    val wirePerson = wire.Person(
      "John",
      "Doe",
      List(
        wire.PaymentMethod.Cash,
        wire.PaymentMethod.PayPal("john@doe.com"),
        wire.PaymentMethod.Card("J. Doe", 23232323)
      )
    )
    
    import io.github.arainko.ducktape.*
    
    pprint.pprintln(
      wirePerson.to[domain.Person]
    )
    // expected output:
    // Person(
    //   firstName = "John",
    //   lastName = "Doe",
    //   paymentMethods = Vector(
    //     Cash,
    //     PayPal(email = "john@doe.com"),
    //     Card(digits = 23232323L, name = "J. Doe")
    //   )
    // )
    
    pprint.pprintln(
      wirePerson
        .into[domain.Person]
        .transform(
          Field.const(_.paymentMethods.element.at[domain.PaymentMethod.PayPal].email, "overridden@email.com")
        )
    )
    // expected output:
    // Person(
    //   firstName = "John",
    //   lastName = "Doe",
    //   paymentMethods = Vector(
    //     Cash,
    //     PayPal(email = "overridden@email.com"),
    //     Card(digits = 23232323L, name = "J. Doe")
    //   )
    // )
    
    pprint.pprintln(
      wirePerson.via(domain.Person.apply)
    )
    // expected output:
    // Person(
    //   firstName = "John",
    //   lastName = "Doe",
    //   paymentMethods = Vector(
    //     Cash,
    //     PayPal(email = "john@doe.com"),
    //     Card(digits = 23232323L, name = "J. Doe")
    //   )
    // )
    
    pprint.pprintln(
      wirePerson
        .intoVia(domain.Person.apply)
        .transform(Field.const(_.paymentMethods.element.at[domain.PaymentMethod.PayPal].email, "overridden@email.com"))
    )
    // expected output:
    // Person(
    //   firstName = "John",
    //   lastName = "Doe",
    //   paymentMethods = Vector(
    //     Cash,
    //     PayPal(email = "overridden@email.com"),
    //     Card(digits = 23232323L, name = "J. Doe")
    //   )
    // )
    ```
    
    Chimney's counterpart:
    
    ```scala
    // file: snippet.scala - part of Ductape counterpart 1
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    object wire:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: List[wire.PaymentMethod]
      )
    
      enum PaymentMethod:
        case Card(name: String, digits: Long)
        case PayPal(email: String)
        case Cash
    
    object domain:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: Vector[domain.PaymentMethod]
      )
    
      enum PaymentMethod:
        case PayPal(email: String)
        case Card(digits: Long, name: String)
        case Cash
    
    @main def example: Unit = {
    val wirePerson = wire.Person(
      "John",
      "Doe",
      List(
        wire.PaymentMethod.Cash,
        wire.PaymentMethod.PayPal("john@doe.com"),
        wire.PaymentMethod.Card("J. Doe", 23232323)
      )
    )
    
    import io.scalaland.chimney.dsl.*
    
    pprint.pprintln(
      wirePerson.transformInto[domain.Person]
    )
    // expected output:
    // Person(
    //   firstName = "John",
    //   lastName = "Doe",
    //   paymentMethods = Vector(
    //     Cash,
    //     PayPal(email = "john@doe.com"),
    //     Card(digits = 23232323L, name = "J. Doe")
    //   )
    // )
    
    pprint.pprintln(
      wirePerson
        .into[domain.Person]
        .withFieldConst(_.paymentMethods.everyItem.matching[domain.PaymentMethod.PayPal].email, "overridden@email.com")
        .transform
    )
    // expected output:
    // Person(
    //   firstName = "John",
    //   lastName = "Doe",
    //   paymentMethods = Vector(
    //     Cash,
    //     PayPal(email = "overridden@email.com"),
    //     Card(digits = 23232323L, name = "J. Doe")
    //   )
    // )
    
    pprint.pprintln(
      wirePerson
        .into[domain.Person]
        .withConstructor(domain.Person.apply)
        .transform
    )
    // expected output:
    // Person(
    //   firstName = "John",
    //   lastName = "Doe",
    //   paymentMethods = Vector(
    //     Cash,
    //     PayPal(email = "john@doe.com"),
    //     Card(digits = 23232323L, name = "J. Doe")
    //   )
    // )
    
    pprint.pprintln(
      wirePerson
        .into[domain.Person]
        .withConstructor(domain.Person.apply)
        .withFieldConst(_.paymentMethods.everyItem.matching[domain.PaymentMethod.PayPal].email, "overridden@email.com")
        .transform
    )
    // expected output:
    // Person(
    //   firstName = "John",
    //   lastName = "Doe",
    //   paymentMethods = Vector(
    //     Cash,
    //     PayPal(email = "overridden@email.com"),
    //     Card(digits = 23232323L, name = "J. Doe")
    //   )
    // )
    }
    ```

!!! example "Nested enum with missing counterpart"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.github.arainko::ducktape::{{ libraries.ducktape }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    object wire:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: List[wire.PaymentMethod]
      )
    
      enum PaymentMethod:
        case Card(name: String, digits: Long)
        case PayPal(email: String)
        case Cash
        case Transfer(accountNo: String) // <-- additional enum case, not present in the domain model
    
    object domain:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: Vector[domain.PaymentMethod]
      )
    
      enum PaymentMethod:
        case PayPal(email: String)
        case Card(digits: Long, name: String)
        case Cash
    
    val wirePerson = wire.Person(
      "John",
      "Doe",
      List(
        wire.PaymentMethod.Cash,
        wire.PaymentMethod.PayPal("john@doe.com"),
        wire.PaymentMethod.Card("J. Doe", 23232323)
      )
    )
    
    import io.github.arainko.ducktape.*
    
    pprint.pprintln(
      wirePerson
        .into[domain.Person]
        .transform(
          Field.const(_.firstName, "Jane"),
          Case.const(_.paymentMethods.element.at[wire.PaymentMethod.Transfer], domain.PaymentMethod.Cash)
        )
    )
    // expected output:
    // Person(
    //   firstName = "Jane",
    //   lastName = "Doe",
    //   paymentMethods = Vector(
    //     Cash,
    //     PayPal(email = "john@doe.com"),
    //     Card(digits = 23232323L, name = "J. Doe")
    //   )
    // )
    ```

    Chimney allows handling nested sealed subtypes/enum cases when by using
    `.withFieldComputedFrom`/'.withFieldComputedPartial`/`.withFieldRenamed` using proper selectors:

    ```scala
    // file: snippet.scala - part of Ductape counterpart 2
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    object wire:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: List[wire.PaymentMethod]
      )
    
      enum PaymentMethod:
        case Card(name: String, digits: Long)
        case PayPal(email: String)
        case Cash
        case Transfer(accountNo: String) // <-- additional enum case, not present in the domain model
    
    object domain:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: Vector[domain.PaymentMethod]
      )
    
      enum PaymentMethod:
        case PayPal(email: String)
        case Card(digits: Long, name: String)
        case Cash
    
    @main def example: Unit = {
    val wirePerson = wire.Person(
      "John",
      "Doe",
      List(
        wire.PaymentMethod.Cash,
        wire.PaymentMethod.PayPal("john@doe.com"),
        wire.PaymentMethod.Card("J. Doe", 23232323)
      )
    )
    
    import io.scalaland.chimney.dsl.*
    
    pprint.pprintln(
      wirePerson
        .into[domain.Person]
        .withFieldConst(_.firstName, "Jane")
        .withFieldComputedFrom(_.paymentMethods.everyItem.matching[wire.PaymentMethod.Transfer])(
          _.paymentMethods.everyItem,
          _ => domain.PaymentMethod.Cash
        )
        .transform
    )
    // expected output:
    // Person(
    //   firstName = "Jane",
    //   lastName = "Doe",
    //   paymentMethods = Vector(
    //     Cash,
    //     PayPal(email = "john@doe.com"),
    //     Card(digits = 23232323L, name = "J. Doe")
    //   )
    // )
    }
    ```

!!! example "Providing missing values"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.github.arainko::ducktape::{{ libraries.ducktape }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    object wire:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: List[wire.PaymentMethod]
      )
    
      enum PaymentMethod:
        case Card(name: String, digits: Long)
        case PayPal(email: String)
        case Cash
    
    object domain:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: Vector[domain.PaymentMethod]
      )
    
      enum PaymentMethod:
        case PayPal(email: String)
        case Card(digits: Long, name: String)
        case Cash
        
    case class PaymentBand(name: String, digits: Long, color: String = "red")
    
    val card: wire.PaymentMethod.Card =
      wire.PaymentMethod.Card(name = "J. Doe", digits = 213712345)
      
    import io.github.arainko.ducktape.*
    
    pprint.pprintln(
      card
        .into[PaymentBand]
        .transform(Field.const(_.color, "blue"))
    )
    // expected output:
    // PaymentBand(name = "J. Doe", digits = 213712345L, color = "blue")
    
    pprint.pprintln(
      card
        .into[PaymentBand]
        .transform(
          Field.computed(_.color, card => if (card.digits % 2 == 0) "green" else "yellow")
        )
    )
    // expected output:
    // PaymentBand(name = "J. Doe", digits = 213712345L, color = "yellow")
    
    pprint.pprintln(
      card
        .into[PaymentBand]
        .transform(Field.default(_.color))
    )
    // expected output:
    // PaymentBand(name = "J. Doe", digits = 213712345L, color = "red")
    
    pprint.pprintln(
      card
        .into[PaymentBand]
        .transform(Field.fallbackToDefault)
    )
    // expected output:
    // PaymentBand(name = "J. Doe", digits = 213712345L, color = "red")
    
    case class SourceToplevel(level1: SourceLevel1, transformable: Option[Int])
    case class SourceLevel1(str: String)
    
    case class DestToplevel(level1: DestLevel1, extra: Option[Int], transformable: Option[Int])
    case class DestLevel1(extra: Option[String], str: String)
    
    val source = SourceToplevel(SourceLevel1("str"), Some(400))
    
    pprint.pprintln(
      source
        .into[DestToplevel]
        .transform(Field.fallbackToNone)
    )
    // expected output:
    // DestToplevel(
    //   level1 = DestLevel1(extra = None, str = "str"),
    //   extra = None,
    //   transformable = Some(value = 400)
    // )

    pprint.pprintln(
      source
        .into[DestToplevel]
        .transform(
          Field.fallbackToNone.regional(
            _.level1
          ), // <-- we're applying the config starting on the `.level1` field and below, it'll be also applied to other transformations nested inside
          Field.const(_.extra, Some(123)) // <-- note that this field now needs to be configured manually
        )
    )
    // expected output:
    // DestToplevel(
    //   level1 = DestLevel1(extra = None, str = "str"),
    //   extra = Some(value = 123),
    //   transformable = Some(value = 400)
    // )
    ```
    
    Chimney's counterpart:
    
    ```scala
    // file: snippet.scala - part of Ductape counterpart 3
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    object wire:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: List[wire.PaymentMethod]
      )
    
      enum PaymentMethod:
        case Card(name: String, digits: Long)
        case PayPal(email: String)
        case Cash
        case Transfer(accountNo: String) // <-- additional enum case, not present in the domain model
    
    object domain:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: Vector[domain.PaymentMethod]
      )
    
      enum PaymentMethod:
        case PayPal(email: String)
        case Card(digits: Long, name: String)
        case Cash
        
    case class PaymentBand(name: String, digits: Long, color: String = "red")
    
    @main def example: Unit = {
    val card: wire.PaymentMethod.Card =
      wire.PaymentMethod.Card(name = "J. Doe", digits = 213712345)
      
    import io.scalaland.chimney.dsl.*
    
    pprint.pprintln(
      card
        .into[PaymentBand]
        .withFieldConst(_.color, "blue")
        .transform
    )
    // expected output:
    // PaymentBand(name = "J. Doe", digits = 213712345L, color = "blue")
    
    pprint.pprintln(
      card
        .into[PaymentBand]
        .withFieldComputed(_.color, card => if (card.digits % 2 == 0) "green" else "yellow")
        .transform
    )
    // expected output:
    // PaymentBand(name = "J. Doe", digits = 213712345L, color = "yellow")
    
    // Default values can only be enabled for a whole derivation, not for a particular field!
    
    pprint.pprintln(
      card
        .into[PaymentBand]
        .enableDefaultValues
        .transform
    )
    // expected output:
    // PaymentBand(name = "J. Doe", digits = 213712345L, color = "red")
    
    case class SourceToplevel(level1: SourceLevel1, transformable: Option[Int])
    case class SourceLevel1(str: String)
    
    case class DestToplevel(level1: DestLevel1, extra: Option[Int], transformable: Option[Int])
    case class DestLevel1(extra: Option[String], str: String)
    
    val source = SourceToplevel(SourceLevel1("str"), Some(400))
    
    pprint.pprintln(
      source
        .into[DestToplevel]
        .enableOptionDefaultsToNone
        .transform
    )
    // expected output:
    // DestToplevel(
    //   level1 = DestLevel1(extra = None, str = "str"),
    //   extra = None,
    //   transformable = Some(value = 400)
    // )
    
    pprint.pprintln(
      source
        .into[DestToplevel]
        .withTargetFlag(_.level1).enableOptionDefaultsToNone
        .withFieldConst(_.extra, Some(123))
        .transform
    )
    // expected output:
    // DestToplevel(
    //   level1 = DestLevel1(extra = None, str = "str"),
    //   extra = Some(value = 123),
    //   transformable = Some(value = 400)
    // )
    }
    ```

!!! example "Providing missing values in nested fields"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.github.arainko::ducktape::{{ libraries.ducktape }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class SourceToplevel1(level1: Option[SourceLevel1])
    case class SourceLevel1(level2: Option[SourceLevel2])
    case class SourceLevel2(int: Int)

    case class DestToplevel1(level1: Option[DestLevel1])
    case class DestLevel1(level2: Option[DestLevel2])
    case class DestLevel2(int: Long)

    val source = SourceToplevel1(Some(SourceLevel1(Some(SourceLevel2(1)))))
    
    import io.github.arainko.ducktape.*

    pprint.pprintln(
      source
        .into[DestToplevel1]
        .transform(
          Field.computedDeep(
            _.level1.element.level2.element.int,
            // the type here cannot be inferred automatically and needs to be provided by the user,
            // a nice compiletime error message is shown (with a suggestion on what the proper type to use is) otherwise
            (value: Int) => value + 10L
          )
        )
    )
    // expected output:
    // DestToplevel1(level1 = Some(value = DestLevel1(level2 = Some(value = DestLevel2(int = 11L)))))
    ```

    Chimney's counterpart:

    ```scala
    // file: snippet.scala - part of Ductape counterpart 4
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class SourceToplevel1(level1: Option[SourceLevel1])
    case class SourceLevel1(level2: Option[SourceLevel2])
    case class SourceLevel2(int: Int)

    case class DestToplevel1(level1: Option[DestLevel1])
    case class DestLevel1(level2: Option[DestLevel2])
    case class DestLevel2(int: Long)

    val source = SourceToplevel1(Some(SourceLevel1(Some(SourceLevel2(1)))))

    import io.scalaland.chimney.dsl.*

    @main def example: Unit = {
    pprint.pprintln(
      source
        .into[DestToplevel1]
        .withFieldComputedFrom(_.level1.matchingSome.level2.matchingSome.int)( // from which field
          _.level1.matchingSome.level2.matchingSome.int, // into which field
          value => value + 10L
        )
        .transform
    )
    // expected output:
    // DestToplevel1(level1 = Some(value = DestLevel1(level2 = Some(value = DestLevel2(int = 11L)))))
    }  
    ```

!!! example "Coproduct configurations"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.github.arainko::ducktape::{{ libraries.ducktape }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    object wire:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: List[wire.PaymentMethod]
      )
    
      enum PaymentMethod:
        case Card(name: String, digits: Long)
        case PayPal(email: String)
        case Cash
        case Transfer(accountNo: String) // <-- additional enum case, not present in the domain model
    
    object domain:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: Vector[domain.PaymentMethod]
      )
    
      enum PaymentMethod:
        case PayPal(email: String)
        case Card(digits: Long, name: String)
        case Cash
    
    val transfer = wire.PaymentMethod.Transfer("2764262")
    
    val wirePerson = wire.Person(
      "John",
      "Doe",
      List(
        wire.PaymentMethod.Cash,
        wire.PaymentMethod.PayPal("john@doe.com"),
        wire.PaymentMethod.Card("J. Doe", 23232323)
      )
    )
    
    import io.github.arainko.ducktape.*
    
    pprint.pprintln(
      transfer
        .into[domain.PaymentMethod]
        .transform(Case.const(_.at[wire.PaymentMethod.Transfer], domain.PaymentMethod.Cash))
    )
    // expected output:
    // Cash
    
    pprint.pprintln(
      transfer
        .into[domain.PaymentMethod]
        .transform(
          Case.computed(_.at[wire.PaymentMethod.Transfer], transfer => domain.PaymentMethod.Card(name = "J. Doe", digits = transfer.accountNo.toLong))
        )
    )
    // expected output:
    // Card(digits = 2764262L, name = "J. Doe")
    ```

    Chimney's counterpart:

    ```scala
    // file: snippet.scala - part of Ductape counterpart 5
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    object wire:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: List[wire.PaymentMethod]
      )
    
      enum PaymentMethod:
        case Card(name: String, digits: Long)
        case PayPal(email: String)
        case Cash
        case Transfer(accountNo: String) // <-- additional enum case, not present in the domain model
    
    object domain:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: Vector[domain.PaymentMethod]
      )
    
      enum PaymentMethod:
        case PayPal(email: String)
        case Card(digits: Long, name: String)
        case Cash
    
    @main def example: Unit = {
    val transfer = wire.PaymentMethod.Transfer("2764262")
    
    val wirePerson = wire.Person(
      "John",
      "Doe",
      List(
        wire.PaymentMethod.Cash,
        wire.PaymentMethod.PayPal("john@doe.com"),
        wire.PaymentMethod.Card("J. Doe", 23232323)
      )
    )
    
    import io.scalaland.chimney.dsl.*
    
    // Currently, Chimney has no disctintion between "Const" and "Computed" case for handling enum subtypes,
    // and if a handling of nested cases would ever become available, the "Computed" name could be confusing:
    // in withFieldComputed(path, src => value) the src is ALWAYS the whole transformed value, while in case handling
    // we are passing "just" a subtype of a whole value. In nested case handling it would be a subtype of some nested
    // field as opposed to the whole transformed value, so the name "Computed" would be confusing and inconsistent. 

    pprint.pprintln(
      transfer
        .into[domain.PaymentMethod]
        .withEnumCaseHandled[wire.PaymentMethod.Transfer](_ => domain.PaymentMethod.Cash)
        .transform
    )
    // expected output:
    // Cash
    
    pprint.pprintln(
      transfer
        .into[domain.PaymentMethod]
        .withEnumCaseHandled[wire.PaymentMethod.Transfer](transfer => domain.PaymentMethod.Card(name = "J. Doe", digits = transfer.accountNo.toLong))
        .transform
    )
    // expected output:
    // Card(digits = 2764262L, name = "J. Doe")
    }
    ```

The biggest difference might be approach towards transformations that can fail in runtime. Ducktape uses user-provided
`F[_]` in derivation with one of two modes: accumulating errors (which requires computing every value, just to see if
there is an error) or fail-fast (which stops computation on the first error, terminating faster but without data that
would allow user to fix the data in one go). It calls such transformations `FallibleTransformer`s.

Chimney uses one blessed error type: `partial.Result[_]`. It used to have a similar approach with `TransformerF`, but it
was decided that users most of the time used: `Option`s (value absence), `Either[String, _]`s (validation with `String`
error message),`Try` (or another `Throwable`-based error handling) or non-empty collection of these. 
`partial.Result` allows storing errors representing each of these, showing which field produced particular error and
deciding between error accumulating and fail-fast in runtime. It provides utilities to convert to and from
`partial.Result`.

!!! example

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.github.arainko::ducktape::{{ libraries.ducktape }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    object newtypes:
      opaque type NonEmptyString <: String = String
      object NonEmptyString:
        def create(value: String): Either[String, NonEmptyString] =
          Either.cond(!value.isBlank, value, s"not a non-empty string")
    
      opaque type Positive <: Long = Long
      object Positive:
        def create(value: Long): Either[String, Positive] =
          Either.cond(value > 0, value, "not a positive long")

    object wire:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: List[wire.PaymentMethod]
      )
    
      enum PaymentMethod:
        case Card(name: String, digits: Long)
        case PayPal(email: String)
        case Cash

    object domain:
      final case class Person(
        firstName: newtypes.NonEmptyString,
        lastName: newtypes.NonEmptyString,
        paymentMethods: Vector[domain.PaymentMethod]
      )
    
      enum PaymentMethod:
        case PayPal(email: newtypes.NonEmptyString)
        case Card(digits: newtypes.Positive, name: newtypes.NonEmptyString)
        case Cash
        
    val wirePerson = wire.Person(
      "John",
      "Doe",
      List(
        wire.PaymentMethod.Cash,
        wire.PaymentMethod.PayPal("john@doe.com"),
        wire.PaymentMethod.Card("J. Doe", 23232323)
      )
    )

    import io.github.arainko.ducktape.*
    
    // expand the 'create' method into an instance of Transformer.Fallible
    // this is a key component in making those transformations automatic
    given failFastNonEmptyString: Transformer.Fallible[[a] =>> Either[String, a], String, newtypes.NonEmptyString] =
      newtypes.NonEmptyString.create
      
    given failFastPositive: Transformer.Fallible[[a] =>> Either[String, a], Long, newtypes.Positive] =
      newtypes.Positive.create

    pprint.pprintln {
      given Mode.FailFast.Either[String] with {}
      
      wirePerson
        .into[domain.Person]
        .fallible
        .transform(
          Field.fallibleConst(
            _.paymentMethods.element.at[domain.PaymentMethod.PayPal].email,
            newtypes.NonEmptyString.create("overridden@email.com")
          )
        )
    }
    // expected output:
    // Right(
    //   value = Person(
    //     firstName = "John",
    //     lastName = "Doe",
    //     paymentMethods = Vector(
    //       Cash,
    //       PayPal(email = "overridden@email.com"),
    //       Card(digits = 23232323L, name = "J. Doe")
    //     )
    //   )
    // )

    // also declare the same fallible transformer but make it ready for error accumulation
    given accumulatingNonEmptyString: Transformer.Fallible[[a] =>> Either[List[String], a], String, newtypes.NonEmptyString] =
      newtypes.NonEmptyString.create(_).left.map(_ :: Nil)
    
    given accumulatingPositive: Transformer.Fallible[[a] =>> Either[List[String], a], Long, newtypes.Positive] =
      newtypes.Positive.create(_).left.map(_ :: Nil)
      
    pprint.pprintln {
      given Mode.Accumulating.Either[String, List] with {}
    
      wirePerson.fallibleTo[domain.Person]
    }
    // expected output:
    // Right(
    //   value = Person(
    //     firstName = "John",
    //     lastName = "Doe",
    //     paymentMethods = Vector(
    //       Cash,
    //       PayPal(email = "john@doe.com"),
    //       Card(digits = 23232323L, name = "J. Doe")
    //     )
    //   )
    // )
    ``` 
    
    Chimney's counterpart:

    ```scala
    // file: snippet.scala - part of Ductape counterpart 6
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    object newtypes:
      opaque type NonEmptyString <: String = String
      object NonEmptyString:
        def create(value: String): Either[String, NonEmptyString] =
          Either.cond(!value.isBlank, value, s"not a non-empty string")
    
      opaque type Positive <: Long = Long
      object Positive:
        def create(value: Long): Either[String, Positive] =
          Either.cond(value > 0, value, "not a positive long")

    object wire:
      final case class Person(
        firstName: String,
        lastName: String,
        paymentMethods: List[wire.PaymentMethod]
      )
    
      enum PaymentMethod:
        case Card(name: String, digits: Long)
        case PayPal(email: String)
        case Cash

    object domain:
      final case class Person(
        firstName: newtypes.NonEmptyString,
        lastName: newtypes.NonEmptyString,
        paymentMethods: Vector[domain.PaymentMethod]
      )
    
      enum PaymentMethod:
        case PayPal(email: newtypes.NonEmptyString)
        case Card(digits: newtypes.Positive, name: newtypes.NonEmptyString)
        case Cash
        
    @main def example: Unit = {
    val wirePerson = wire.Person(
      "John",
      "Doe",
      List(
        wire.PaymentMethod.Cash,
        wire.PaymentMethod.PayPal("john@doe.com"),
        wire.PaymentMethod.Card("J. Doe", 23232323)
      )
    )
    
    import io.scalaland.chimney.dsl.*
    import io.scalaland.chimney.{partial, PartialTransformer}
    import partial.syntax.*
    
    given PartialTransformer[String, newtypes.NonEmptyString] = PartialTransformer[String, newtypes.NonEmptyString](str =>
      newtypes.NonEmptyString.create(str).asResult
    )
    
    given PartialTransformer[Long, newtypes.Positive] = PartialTransformer[Long, newtypes.Positive](str =>
      newtypes.Positive.create(str).asResult
    )
    
    pprint.pprintln(
      wirePerson.transformIntoPartial[domain.Person].asEitherErrorPathMessageStrings
    )
    pprint.pprintln(
      wirePerson.transformIntoPartial[domain.Person](failFast = true).asEitherErrorPathMessageStrings
    )
    // expected output:
    // Right(
    //   value = Person(
    //     firstName = "John",
    //     lastName = "Doe",
    //     paymentMethods = Vector(
    //       Cash,
    //       PayPal(email = "john@doe.com"),
    //       Card(digits = 23232323L, name = "J. Doe")
    //     )
    //   )
    // )
    // Right(
    //   value = Person(
    //     firstName = "John",
    //     lastName = "Doe",
    //     paymentMethods = Vector(
    //       Cash,
    //       PayPal(email = "john@doe.com"),
    //       Card(digits = 23232323L, name = "J. Doe")
    //     )
    //   )
    // )
    
    pprint.pprintln(
      wirePerson.intoPartial[domain.Person]
        .withFieldConstPartial(
          _.paymentMethods.everyItem.matching[domain.PaymentMethod.PayPal].email,
          newtypes.NonEmptyString.create("overridden@email.com").asResult
        )
        .transform
        .asEitherErrorPathMessageStrings
    )
    pprint.pprintln(
      wirePerson.intoPartial[domain.Person]
        .withFieldConstPartial(
          _.paymentMethods.everyItem.matching[domain.PaymentMethod.PayPal].email,
          newtypes.NonEmptyString.create("overridden@email.com").asResult
        )
        .transformFailFast
        .asEitherErrorPathMessageStrings
    )
    // expected output:
    // Right(
    //   value = Person(
    //     firstName = "John",
    //     lastName = "Doe",
    //     paymentMethods = Vector(
    //       Cash,
    //       PayPal(email = "overridden@email.com"),
    //       Card(digits = 23232323L, name = "J. Doe")
    //     )
    //   )
    // )
    // Right(
    //   value = Person(
    //     firstName = "John",
    //     lastName = "Doe",
    //     paymentMethods = Vector(
    //       Cash,
    //       PayPal(email = "overridden@email.com"),
    //       Card(digits = 23232323L, name = "J. Doe")
    //     )
    //   )
    // )
    }
    ``` 

Since Ducktape is inspired by Chimney, there is a huge overlap in functionality. However, there are some differences:

 * Ducktape is developed only on Scala 3, while Chimney supports 2.12 and 2.13 as well
 * Ducktape provides support to arbitrary effect `F[_]` through `Fallible[F]` combined with 2 modes of derivation:
   `Mode.Accumulating[F]`  (aggregating errors from different fields, basically `Applicative`/`Parallel`) and
   `Mode.FailFast[F]` (terminating on the first error, basically `Monad`). Chimney supports one, dedicated and optimized
   result type `partial.Result` which: can be switched between aggregating/fail-fast mode in runtime, stores path to
   failed field/index/map key, catches `Exception`s and handles `None`
 * Ducktape takes all overrides as values passed into `inline def` macro which can remove intermediate values from the
   final code, while Chimney uses fluent API (builder) which comes with a small runtime overhead
 * Ducktape provides/allows:
    * some linting telling the users that they overrode some things twice, or that some config cannot be used because
      another config provided value for level "above"
    * `Field.allMatching` which has no direct counterpart in Chimney, but in many cases it could replaced using
      [`withFallback`](supported-transformations.md#merging-multiple-input-sources-into-a-single-target-value),
      using data from `allMatching` as the source, and source of Ducktape transformation as a fallback
 * Chimney provides/allows:
    * [conversions to/from tuples](supported-transformations.md#frominto-a-tuple)
    * reading to/from Java Bean accessors ([getters](supported-transformations.md#reading-from-bean-getters) and
      [setters](supported-transformations.md#writing-to-non-unit-bean-setters))
    * [Java enums](supported-transformations.md#javas-enums) support
    * support to [custom optionals](cookbook.md#custom-optional-types) and
      [collections](cookbook.md#custom-collection-types) in such a way the `_.matchingSome`, `_.everyItem`,
      `_.everyMapKey` and `_.everyMapValue` would work with them (including Java collections and Cats data)
    * customizing the [field-](supported-transformations.md#customizing-field-name-matching) and
      [subtype-name](supported-transformations.md#customizing-subtype-name-matching) matching methods
    * [sharing flags overrides between all derivations in the same scope](cookbook.md#reusing-flags-for-several-transformationspatchings)
    * [smart constructors](supported-transformations.md#types-with-manually-provided-constructors), not only custom
      constructors guaranteed to create the value
    * [`Patcher`s](supported-patching.md)

which means each library can bring something unique to the table.

## Compilation errors

When some transformation cannot be generated with the information available to the library, it is perfectly normal that
a macro would generate a compilation error with a message describing the issue.

However, some compilation errors might seem unreasonable as everything seems to be configured correctly for a use case
that is officially supported.

### Recursive types fail to compile

Chimney attempts to avoid unnecessary memory allocations for good performance.

It means that the code `foo.into[Bar].transform` would try to avoid the creation of
`Transformer[Foo, Bar]` - if the user provided one it would have to be used, but if
the only available implicit would come from automatic derivation, it would be ignored so
that macro would generate an inlined expression.

This isn't possible with recursive types, as you cannot inline potentially unbounded
nesting of transformations. For them, it is suggested to derive the `Transformer`,
assigning it to `implicit val`/`implicit def` so that recursive transformations would
be handled by recursive calls. This can be done with:

!!! example

    ```scala
    implicit val foo2bar: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]

    // or

    implicit val foo2bar: Transformer[Foo, Bar] = Transformer.define[Foo, Bar].buildTransformer
    ```

and then

!!! example

    ```scala
    foo.transformInto[Bar] // uses implicit Transformer (with recursive transformation)
    ```

The same is true for partial transformers.

### Recursive calls on implicits

Old versions of Chimney in situations like this:

!!! example

    ```scala
    implicit val t: Transformer[Foo, Bar] = foo => foo.transformInto[Bar] // or
    implicit val t: Transformer[Foo, Bar] = foo => foo.into[Bar].transform
    ```
    
would result in errors like:

!!! example

    ```
    forward reference extends over definition of value t
    ```

In newer, it can result in errors like:

!!! example

    ```scala
    java.lang.StackOverflowError
    ```

It's a sign of recursion which has to be handled with [semiautomatic derivation](cookbook.md#automatic-vs-semiautomatic).

!!! example

    ```scala
    implicit val t: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar] // or
    implicit val t: Transformer[Foo, Bar] = Transformer.define[Foo, Bar].buildTransformer
    ```

### `sealed trait`s fail to recompile

In the case of incremental compilation, the Zinc compiler sometimes has issues with
caching certain kind of information and macros don't get proper information
from `knownDirectSubclasses` method. It usually helps when you `clean`
and `compile` again. It cannot be fixed in the library as it relies on
the compiler to provide it with this data, and the compiler fails to do so.

On Scala 2.12.0 it failed [in other cases as well (scala/bug#7046)](https://github.com/scala/bug/issues/7046),
so it is recommended to update 2.12 to at least 2.12.1.

### Scala 3 complains that `implicit`/`given` `TransformerConfiguration` needs an explicit return type

In Scala 2 syntax like

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    implicit def cfg = TransformerConfiguration.default.enableMacrosLogging
    ```

was perfectly OK. Using implicits without a type was a bad practice but not an error. 

This changes in Scala 3 where you'll get an error:

!!! example
    
    ```
    result type of implicit definition needs to be given explicitly
    ```

You can work around this by slightly longer incantation:

!!! example
 
    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    transparent inline given TransformerConfiguration[?] =
      TransformerConfiguration.default.enableMacrosLogging
    ```

### `java.lang.UnsupportedOperationException: Position.point on NoPosition` error

On Scala 2 `java.lang.UnsupportedOperationException: Position.point on NoPosition` is most commonly seen due to
[scala/bug#10604](https://github.com/scala/bug/issues/10604) - when JVM used for compilation has a small stack trace
recursive derivation (not only in Chimney) can overflow this stack trace, but on Scala 2 it can become notorious in
the form of an empty value used by the macro to report where error happened.

These issues can be addressed by increasing the compiler's JVM stack size, passing it e.g. `-Xss64m` (to increase
the size to 64MB).

However, if you are using the compiler's flags to report unused definitions when macros are involved, there can also be
an error caused by [scala/bug#12895](https://github.com/scala/bug/issues/12895). This bug was fixed in Scala 2.13.14,
if update is impossible the workaround would be to remove the unused definition reporting.

### Debugging macros

In some cases, it could be helpful to preview what is the expression generated
by macros, which implicits were used in macro (or not) and what was the exact
logic that lead to the final expression or compilation errors.

In such cases, we can use a dedicated flag, `.enableMacrosLogging`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class Foo(x: String, y: Int, z: Boolean = true)
    case class Bar(x: String, y: Int)

    Bar("abc", 10).into[Foo].enableDefaultValues.enableMacrosLogging.transform
    ```

For the snippet above, the macro could print this structured log:

!!! example

    ```
    + Start derivation with context: ForTotal[From = Bar, To = Foo](src = bar)(TransformerConfig(
    |   flags = TransformerFlags(processDefaultValues, displayMacrosLogging),
    |   instanceFlagOverridden = true,
    |   fieldOverrides = Map(),
    |   coproductOverrides = Map(),
    |   preventImplicitSummoningForTypes = None
    | ))
    + Deriving Total Transformer expression from Bar to Foo with context:
    | ForTotal[From = Bar, To = Foo](src = bar)(TransformerConfig(
    |   flags = TransformerFlags(processDefaultValues, displayMacrosLogging),
    |   instanceFlagOverridden = true,
    |   fieldOverrides = Map(),
    |   coproductOverrides = Map(),
    |   preventImplicitSummoningForTypes = None
    | ))
      + Attempting expansion of rule Implicit
      + Rule Implicit decided to pass on to the next rule - some conditions were fulfilled but at least one failed: Configuration has defined overrides
      + Attempting expansion of rule Subtypes
      + Rule Subtypes decided to pass on to the next rule
      + Attempting expansion of rule OptionToOption
      + Rule OptionToOption decided to pass on to the next rule
      + Attempting expansion of rule PartialOptionToNonOption
      + Rule PartialOptionToNonOption decided to pass on to the next rule
      + Attempting expansion of rule ToOption
      + Rule ToOption decided to pass on to the next rule
      + Attempting expansion of rule ValueClassToValueClass
      + Rule ValueClassToValueClass decided to pass on to the next rule
      + Attempting expansion of rule ValueClassToType
      + Rule ValueClassToType decided to pass on to the next rule
      + Attempting expansion of rule TypeToValueClass
      + Rule TypeToValueClass decided to pass on to the next rule
      + Attempting expansion of rule EitherToEither
      + Rule EitherToEither decided to pass on to the next rule
      + Attempting expansion of rule MapToMap
      + Rule MapToMap decided to pass on to the next rule
      + Attempting expansion of rule IterableToIterable
      + Rule IterableToIterable decided to pass on to the next rule
      + Attempting expansion of rule ProductToProduct
    + Resolved Bar getters: (`x`: java.lang.String (ConstructorVal, declared), `y`: scala.Int (ConstructorVal, declared), `_1`: java.lang.String (AccessorMethod, declared), `_2`: scala.Int (AccessorMethod, declared)) and Foo constructor (`x`: java.lang.String (ConstructorParameter, default = None), `y`: scala.Int (ConstructorParameter, default = None), `z`: scala.Boolean (ConstructorParameter, default = Some(Foo.$lessinit$greater$default)))
    + Recursive derivation for field `x`: java.lang.String into matched `x`: java.lang.String
      + Deriving Total Transformer expression from java.lang.String to java.lang.String with context:
      | ForTotal[From = java.lang.String, To = java.lang.String](src = bar.x)(TransformerConfig(
      |   flags = TransformerFlags(processDefaultValues, displayMacrosLogging),
      |   instanceFlagOverridden = false,
      |   fieldOverrides = Map(),
      |   coproductOverrides = Map(),
      |   preventImplicitSummoningForTypes = None
      | ))
    + Attempting expansion of rule Implicit
    + Rule Implicit decided to pass on to the next rule
    + Attempting expansion of rule Subtypes
    + Rule Subtypes expanded successfully: bar.x
      + Derived recursively total expression bar.x
    + Resolved `x` field value to bar.x
    + Recursive derivation for field `y`: scala.Int into matched `y`: scala.Int
      + Deriving Total Transformer expression from scala.Int to scala.Int with context:
      | ForTotal[From = scala.Int, To = scala.Int](src = bar.y)(TransformerConfig(
      |   flags = TransformerFlags(processDefaultValues, displayMacrosLogging),
      |   instanceFlagOverridden = false,
      |   fieldOverrides = Map(),
      |   coproductOverrides = Map(),
      |   preventImplicitSummoningForTypes = None
      | ))
    + Attempting expansion of rule Implicit
    + Rule Implicit decided to pass on to the next rule
    + Attempting expansion of rule Subtypes
    + Rule Subtypes expanded successfully: bar.y
      + Derived recursively total expression bar.y
    + Resolved `y` field value to bar.y
    + Resolved `z` field value to Foo.$lessinit$greater$default
    + Resolved 3 arguments, 3 as total and 0 as partial Expr
      + Rule ProductToProduct expanded successfully: new Foo(bar.x, bar.y, Foo.$lessinit$greater$default)
    + Derived final expression is:
    | new Foo(bar.x, bar.y, Foo.$lessinit$greater$default)
    + Derivation took 0.072478000 s
    ```

With the structured log, the user could see e.g.:

  - that no implicit was summoned during the expansion
  - how `Foo` constructor was called
  - that default values were used and how they were obtained
  - what is the final expression and how long it took to compute it

!!! warning

    Structured logs from macros are still logs - their role is to help with
    debugging, but their format changes over time and the log for one macro could
    look completely different from the log from another macro. Examples from this
    page should not be treated as any point of reference.

Enabling logs can be done both on an individual transformation level, like
above, or with a shared implicit config:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableMacrosLogging
    ```

The flag is also available to `Patcher`s, this code:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class Email(address: String) extends AnyVal
    case class Phone(number: Long) extends AnyVal

    case class User(id: Int, email: Email, phone: Phone)
    case class UserUpdateForm(email: String, phone: Long)

    val user = User(10, Email("abc@@domain.com"), Phone(1234567890L))
    val updateForm = UserUpdateForm("xyz@@domain.com", 123123123L)

    user.using(updateForm).enableMacrosLogging.patch
    ```

would generate:

!!! example

    ```
    + Start derivation with context: PatcherContext[A = User, Patch = UserUpdateForm](obj = user, patch = userupdateform)(PatcherConfig(
    |   flags = PatcherFlags(displayMacrosLogging),
    |   preventImplicitSummoningForTypes = None
    | ))
    + Deriving Patcher expression for User with patch UserUpdateForm
      + Deriving Total Transformer expression from java.lang.String to Email with context:
      | ForTotal[From = java.lang.String, To = Email](src = userupdateform.email)(TransformerConfig(
      |   flags = TransformerFlags(),
      |   instanceFlagOverridden = false,
      |   fieldOverrides = Map(),
      |   coproductOverrides = Map(),
      |   preventImplicitSummoningForTypes = None
      | ))
    + Attempting expansion of rule Implicit
    + Rule Implicit decided to pass on to the next rule
    + Attempting expansion of rule Subtypes
    + Rule Subtypes decided to pass on to the next rule
    + Attempting expansion of rule OptionToOption
    + Rule OptionToOption decided to pass on to the next rule
    + Attempting expansion of rule PartialOptionToNonOption
    + Rule PartialOptionToNonOption decided to pass on to the next rule
    + Attempting expansion of rule ToOption
    + Rule ToOption decided to pass on to the next rule
    + Attempting expansion of rule ValueClassToValueClass
    + Rule ValueClassToValueClass decided to pass on to the next rule
    + Attempting expansion of rule ValueClassToType
    + Rule ValueClassToType decided to pass on to the next rule
    + Attempting expansion of rule TypeToValueClass
      + Deriving Total Transformer expression from java.lang.String to java.lang.String with context:
      | ForTotal[From = java.lang.String, To = java.lang.String](src = userupdateform.email)(TransformerConfig(
      |   flags = TransformerFlags(),
      |   instanceFlagOverridden = false,
      |   fieldOverrides = Map(),
      |   coproductOverrides = Map(),
      |   preventImplicitSummoningForTypes = None
      | ))
    + Attempting expansion of rule Implicit
    + Rule Implicit decided to pass on to the next rule
    + Attempting expansion of rule Subtypes
    + Rule Subtypes expanded successfully: userupdateform.email
      + Derived recursively total expression userupdateform.email
    + Rule TypeToValueClass expanded successfully: new Email(userupdateform.email)
      + Deriving Total Transformer expression from scala.Long to Phone with context:
      | ForTotal[From = scala.Long, To = Phone](src = userupdateform.phone)(TransformerConfig(
      |   flags = TransformerFlags(),
      |   instanceFlagOverridden = false,
      |   fieldOverrides = Map(),
      |   coproductOverrides = Map(),
      |   preventImplicitSummoningForTypes = None
      | ))
    + Attempting expansion of rule Implicit
    + Rule Implicit decided to pass on to the next rule
    + Attempting expansion of rule Subtypes
    + Rule Subtypes decided to pass on to the next rule
    + Attempting expansion of rule OptionToOption
    + Rule OptionToOption decided to pass on to the next rule
    + Attempting expansion of rule PartialOptionToNonOption
    + Rule PartialOptionToNonOption decided to pass on to the next rule
    + Attempting expansion of rule ToOption
    + Rule ToOption decided to pass on to the next rule
    + Attempting expansion of rule ValueClassToValueClass
    + Rule ValueClassToValueClass decided to pass on to the next rule
    + Attempting expansion of rule ValueClassToType
    + Rule ValueClassToType decided to pass on to the next rule
    + Attempting expansion of rule TypeToValueClass
      + Deriving Total Transformer expression from scala.Long to scala.Long with context:
      | ForTotal[From = scala.Long, To = scala.Long](src = userupdateform.phone)(TransformerConfig(
      |   flags = TransformerFlags(),
      |   instanceFlagOverridden = false,
      |   fieldOverrides = Map(),
      |   coproductOverrides = Map(),
      |   preventImplicitSummoningForTypes = None
      | ))
    + Attempting expansion of rule Implicit
    + Rule Implicit decided to pass on to the next rule
    + Attempting expansion of rule Subtypes
    + Rule Subtypes expanded successfully: userupdateform.phone
      + Derived recursively total expression userupdateform.phone
    + Rule TypeToValueClass expanded successfully: new Phone(userupdateform.phone)
    + Derived final expression is:
    | new User(user.id, new Email(userupdateform.email), new Phone(userupdateform.phone))
    + Derivation took 0.113354000 s
    ```

## More sources, videos and tutorials

Videos/presentations including or describing Chimney:

 * [*Domain, API, DTO - translating between layers with Chimney*, Mateusz Kubuszok](https://www.youtube.com/watch?v=SNc7xeHrKnQ&t=194s) ([slides](https://mateuszkubuszok.github.io/DomainApiDtoChimney/#/)) - showing examples in newer versions of Chimney (1.0.0+)
 * [*Breaking framework chains with vanilla Scala*, Marcin Szałomski](https://www.youtube.com/watch?v=U67BAeH3cxo)
 * [*Unveiling the Magic: Chimney’s Internals, Macros & Scala 3*, Mateusz Kubuszok](https://mateuszkubuszok.github.io/ChimneyInternalsPresentation/#/) - unfortunatelly not recorded, the presentation explained what Chimney does under the hood and why
   migration from 0.7.x (Scala 2-only) to 0.8.x (Scala 2 + Scala 3) was such a challenge (it wasn't only about macros)
 * [*The Best Scala Libraries I Use in Every Project*, DevInsideYou](https://www.youtube.com/watch?v=ZymD5NuOwdA&t=194s) - mentions Chimney
 * [*Data Juggling - Part 1 - Getting Started with #Chimney*, DevInsideYou](https://www.youtube.com/watch?v=ezz0BpEHEQY) - examples with older versions of Chimney (before `PartialTransformer`s introduction, `TransformerF` deprecation and 1.0.0 stabilization, but the main idea remained unchanged)

Articles/written tutorials including or describing Chimney:

 * [*5 Scala Libraries That Will Make Your Life Easier*, Jacek Kunicki](https://softwaremill.com/5-scala-libraries-that-will-make-your-life-easier/) - mentions Chimney as #2
 * [*Introduction to Chimney*, Brad Ovitt](https://www.baeldung.com/scala/chimney-data-transformation-library) - tutorial

## Ideas, questions or bug reports

If you haven't found a solution to your question try asking
at [GitHub discussions](https://github.com/scalalandio/chimney/discussions) page. If your idea looks like a
feature request or a bug, open an issue at [GitHub issues](https://github.com/scalalandio/chimney/issues) page.
