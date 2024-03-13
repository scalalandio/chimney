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
  - want to update immutable data by passing some path to the updated field and then provide a value - you need a lens
    library like Quicklens or Monocle
  - want to limit the amount of tests written and are wondering if automatic generation of such an important code is
    safe - you need to ask yourself: would you have the same dilemma if you were asked about generating JSON codecs?
    Would you wonder if you need to test them? (In our experience, yes). Could you remove the need to test them if you
    mandated the code to be always/never generated? (In our experience, whether its generated or written by hand you
    want to test it).
    - You can however avoid testing someone else's library if you use Chimney in place, in some service, and then test
      that service's behavior or if you create in your DTO model `def toDomain = this.transformInto[DomainModel]`.
      You can utilize code generation without making your application's type signatures depend on someone else's types. 

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
    your custom type `F[T]`, while in Partial Transformers they work with `partial.Result[T]`. See the
    `partial.Result` companion object for ways of constructing `success` and `failure` instances, for example:
    - `partial.Result.fromValue`
    - `partial.Result.fromOption`
    - `partial.Result.fromEither`
    - `partial.Result.fromTry`
    - and so on...
  - the resulting type of a call to `.transform` is also a `partial.Result[T]`. If you don't want to work with
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: Option[String])
    case class Bar(a: String)
    
    Foo(Some("value")).into[Bar].enableUnsafeOption.transform // Bar("value")
    Foo(None).into[Bar].enableUnsafeOption.transform // throws Exception
    ```

Throwing exceptions made sense as a workaround in simpler times, when `Transformer`s were the only option. However,
now we have `PartialTransformer`s. They have a build-in ability to unwrap `Option` as failed result.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: Option[String])
    case class Bar(a: String)
    
    Foo(Some("value")).transformIntoPartial[Bar](failFast = true).asOption // Some(Bar("value"))
    Foo(None).transformIntoPartial[Bar](failFast = true).asOption // None
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney._
    
    class MyType[A](private val a: A) {
      def map[B](f: A => B): MyType[B] =
        new MyType(f(a))
    }

    implicit def provideMyType[A, B](
        implicit a2b: Transformer[A, B]
    ): Transformer[MyType[A], MyType[B]] =
      myA => myA.map(_.transformInto[B])
    ```

After changes in 0.8.x `implicit Transformer[A, B]` means "instance provided by user",
either manually or through semiautomatic derivation. If the user wants to allow summoning
there the automatic instances as well, they need to use `Transformer.AutoDerived`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney._
    
    class MyOtherType[A](private val a: A) {
      def map[B](f: A => B): MyOtherType[B] =
        new MyOtherType(f(a))
    }
  
    implicit def provideMyOtherType[A, B](
        implicit a2b: Transformer.AutoDerived[A, B]
    ): Transformer[MyOtherType[A], MyOtherType[B]] =
      myA => myA.map(_.transformInto[B])
    ```

which would summon both automatically derived instances and manually provided ones.
The difference is shown in this example:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    // implicit provided by the user
    implicit val int2str: Transformer[Int, String] = _.toString
  
    val myType: MyType[Int] = new MyType(10)
    val myOtherType: MyOtherType[Int] = new MyOtherType(10)
  
    // uses provideMyType(int2str):
    myType.transformInto[MyType[String]]
  
    // uses provideMyOtherType(int2str):
    myOtherType.transformInto[MyOtherType[String]
  
    val myType2: MyType[Either[Int, Int]] = new MyType(Right(10))
    val myOtherType2: MyOtherType[Either[Int, Int]] = new MyOtherType(Right(10)
  
    // requires manually provided transformer e.g.
    //   implicit val either2either =
    //     Transformer.derive[Either[Int, Int], Either[String, String]]
    // without it, the compilation fails
    // myType2.transformInto[MyType[Either[String, String]]]
  
    // uses provideMyOtherType(Transformer.derive):
    myOtherType2.transformInto[Either[String, String]]
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

    ```scala
    forward reference extends over definition of value t
    ```

In newer, it can result in would result in errors like:

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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    implicit def cfg = TransformerConfiguration.default.enableMacrosLogging
    ```

was perfectly OK. Using implicits without a type was a bad practice but not an error. 

This changes in Scala 3 where you'll get an error:

!!! example
    
    ```scala
    result type of implicit definition needs to be given explicitly
    ```

You can work around this by slightly longer incantation:

!!! example
 
    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    transparent inline given TransformerConfiguration[?] =
      TransformerConfiguration.default.enableMacrosLogging
    ```

### `java.lang.UnsupportedOperationException: Position.point on NoPosition` error

On Scala 2 `java.lang.UnsupportedOperationException: Position.point on NoPosition` is most commonly seen due to
[scala/bug#10604](https://github.com/scala/bug/issues/10604) - when JVM used for compilation has a small stack trace
recursive derivation (not only in Chimney) can overflow this stack trace, but on Scala 2 it can become notorious in
the form of an empty value used by the macro to report where error happened.

These issues can be addressed by increasing the compiler's JVM stack size, passing it e.g. -Xss64m (to increase the size
to 64MB).

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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
     // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableMacrosLogging
    ```

The flag is also available to `Patcher`s, this code:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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

## Ideas, questions or bug reports

If you haven't found a solution to your question try asking
at [GitHub discussions](https://github.com/scalalandio/chimney/discussions) page. If your idea looks like a
feature request or a bug, open an issue at [GitHub issues](https://github.com/scalalandio/chimney/issues) page.
