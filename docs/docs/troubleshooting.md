# Troubleshooting

Already using Chimney and you've got some issue? This page might help you with it.

## Migration from 0.7.x to 0.8.0

Version 0.8.0 is the first version which cleaned up the API. It introduced
several breaking changes.

### Replacing Lifted Transformers (`TransformerF`) with `PartialTransformer`s

Lifted Transformers (`TransformerT`), deprecated in 0.7.0, got removed in favor of `PartialTransformer`s

Chimney's Lifted Transformers were historically the first experimental attempt
to express transformations that may potentially fail. Despite their great expressiveness, they were
lacking several basic features and had a few design flaws that make them unattractive/difficult
for wider adoption.

Let's have a look at type signatures of both Lifted and Partial Transformers.

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
    partial transformers fix resulting type to built-in ``partial.Result[_]``
    - as a consequence of this abstraction, Lifted Transformers required a type class instance
      (`TransformerFSupport`) in scope for every specific `F[_+]` used
    - Partial Transformers rely on built-in behavior and provide convenience methods to convert between more familiar
      data types (`Option`, `Either`, etc.)
    - abstraction over the resulting container type in lifted transformer allowed for having custom error types;
      this is not easily possible with partial transformer, which focuses on few most common error types
  - Partial Transformer has a built-in support for fail-fast (short-circuiting) semantics by passing `failFast`
    boolean parameter, while in Lifted Transformers it was barely possible (only by providing supporting type class
    that had such a fixed behavior)
  - Error path support in Lifted Transformers required providing another type class instance
    (`TransformerFErrorPathSupport`) for your errors collection type,
    while in partial transformers it is a built-in feature

In order to migrate your code from Lifted Transformers to Partial Transformers, you may take the following steps.

  - replace all the occurrences of `TransformerF` type with `PartialTransformer` and remove the first type argument
    (`F[_]`) which is not used for partial transformers.
  - for your transformations find corresponding DSL methods. Their name usually differ on suffix, for example:
    - replace ``withFieldConstF`` with ``withFieldConstPartial``
    - replace ``withFieldComputedF`` with ``withFieldComputedPartial``
    - etc.
  - adjust the types passed to the customization methods. In Lifted Transformers they were expecting values of
    your custom type `F[T]`, while in Partial Transformers they work with `partial.Result[T]`. See the
    `partial.Result` companion object for ways of constructing `success` and `failure` instances, for example:
    - `partial.Result.fromValue`
    - `partial.Result.fromOption`
    - `partial.Result.fromEither`
    - `partial.Result.fromTry`
    - and so on...
  - the resulting type of a call to `.transform` is also a `partial.Result[T]`. If you don't want to work with
    `partial.Result` directly, figure out ways how to convert it to other, more familiar data structures.
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
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: Option[String])
    case class Bar(a: String)
    
    Foo(Some("value")).into[Bar].enableUnsafeOption.transform // Bar("value")
    Foo(None).into[Bar].enableUnsafeOption.transform // throws Exception
    ```

Throwing exceptions made sense as a workaround in simpler times, when `Transformer`s were the only option. However,
now we have `PartialTransformer`s. They have build-in ability to unwrap `Option` as failed result.

!!! example

    ```scala
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
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

It was caused be the change in mechanism for recursive derivation: since Chimney
avoid boxing and allocation where possible, it used to check if summoned
implicit was generated by automatic derivation. If implicit came from
`Transformer.derive` or `PartialTransformer.derive` it was discarded and
the macro attempted to derive it again without wrapping the result in a type class.

It both complicated code and increased compilation times, as each field
or subtype would attempt to summon implicit (potentially triggering macro expansion)
and then discard it if it didn't come from the user. Splitting types allows compiler
to not summon any implicit if the user haven't provided any.

The consequence is only visible if there is some `implicit def` which takes
another implicit `Transformer`.

!!! example

    ```scala
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
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
either manually or through semiautomatic derivation. If users want to allow summoning
there automatic instances as well, they need to use `Transformer.AutoDerived`:

!!! example

    ```scala
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
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
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
    import io.scalaland.chimney.dsl._
    
    // implicit provided by user
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
    // without it, compilation fails
    // myType2.transformInto[MyType[Either[String, String]]]
  
    // uses provideMyOtherType(Transformer.derive):
    myOtherType2.transformInto[Either[String, String]]
    ```

### Default values no longer are used as fallback if source field exists

If:

  - default values were enabled,
  - source and target had a field defined
  - this field had default value defined
  - macro couldn't derive transformation from source field type to target field type

Chimney used to use the default value.

However, this was a buggy behavior, and currently it only uses default values
if there is no source field nor other fallback or override. Although it is
a bugfix, it is also a breaking change so it has to be documented. The fix would
be a manual resolution for all fields which now (correctly) fail due to the bugfix.

## Recursive types fail to compile

Chimney attempts to avoid unnecessary memory allocations for good performance.

It means that the code `foo.into[Bar].transform` would try to avoid creation of
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

## `sealed trait`s fail to recompile

In case of incremental compilation, Zinc compiler sometimes has issues with
caching certain kind of information and macros don't get proper information
from `knownDirectSubclasses` method. It usually helps when you `clean`
and `compile` again. It cannot be fixed in the library as it relies on
the compiler to provide it with this data, and compiler fails to do so.

On Scala 2.12.0 it failed [in other cases as well](https://github.com/scala/bug/issues/7046),
so it is recommended to update 2.12 to at least 2.12.1.

## Debugging macros

In some cases it could be helpful to preview what is the expression generated
by macros, which implicits were used in macro (or not) and what was the exact
logic that lead to the final expression or compilation errors.

In such cases, we can use a dedicated flag, `.enableMacrosLogging`:

!!! example

    ```scala
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
    import io.scalaland.chimney.dsl._

    case class Foo(x: String, y: Int, z: Boolean = true)
    case class Bar(x: String, y: Int)

    Bar("abc", 10).into[Foo].enableDefaultValues.enableMacrosLogging.transform
    ```

For the snippet above, the macro could print this structured log:

!!! example

    ```
    + Start derivation with context: ForTotal[From = Bar, To = Foo](src = bar)(TransformerConfig(
    |   flags = Flags(processDefaultValues, displayMacrosLogging),
    |   fieldOverrides = Map(),
    |   coproductOverrides = Map(),
    |   preventResolutionForTypes = None
    | ))
    + Deriving Total Transformer expression from Bar to Foo
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
      + Rule TypeToValueClass decided to pass on to the next rule
      + Attempting expansion of rule EitherToEither
      + Rule EitherToEither decided to pass on to the next rule
      + Attempting expansion of rule MapToMap
      + Rule MapToMap decided to pass on to the next rule
      + Attempting expansion of rule IterableToIterable
      + Rule IterableToIterable decided to pass on to the next rule
      + Attempting expansion of rule ProductToProduct
        + Resolved Bar getters: (`x`: java.lang.String (ConstructorVal), `y`: scala.Int (ConstructorVal)) and Foo constructor (`x`: java.lang.String (ConstructorParameter, default = None), `y`: scala.Int (ConstructorParameter, default = None), `z`: scala.Boolean (ConstructorParameter, default = Some(Foo.apply$default)))
        + Recursive derivation for field `x`: java.lang.String into matched `x`: java.lang.String
          + Deriving Total Transformer expression from java.lang.String to java.lang.String
            + Attempting expansion of rule Implicit
            + Rule Implicit decided to pass on to the next rule
            + Attempting expansion of rule Subtypes
            + Rule Subtypes expanded successfully: bar.x
          + Derived recursively total expression bar.x
        + Resolved `x` field value to bar.x
        + Recursive derivation for field `y`: scala.Int into matched `y`: scala.Int
        + Deriving Total Transformer expression from scala.Int to scala.Int
          + Attempting expansion of rule Implicit
          + Rule Implicit decided to pass on to the next rule
          + Attempting expansion of rule Subtypes
          + Rule Subtypes expanded successfully: bar.y
        + Derived recursively total expression bar.y
        + Resolved `y` field value to bar.y
        + Resolved `z` field value to Foo.apply$default
        + Resolved 3 arguments, 3 as total and 0 as partial Expr
      + Rule ProductToProduct expanded successfully:
        | new Foo(bar.x, bar.y, Foo.apply$default)
    + Derived final expression is:
      | new Foo(bar.x, bar.y, Foo.apply$default)
    + Derivation took 0.109828000 s
    ```

With the structured log a user could see e.g.:

  - that no implicit was summoned during the expansion
  - how `Foo` constructor was called
  - that default values was used and how it was obtained
  - what is the final expression and how long it took to compute it

!!! warning

    Structured logs from macros are still logs - their role is to help with
    debugging, but their format evolves over time and log for one macro could
    look completely different to log from another macro. Examples from this
    page should not be treated as any point of reference.

Enabling logs can be done both on an individual transformation level, like
above, or with a shared implicit config:

!!! example

    ```scala
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
    import io.scalaland.chimney.dsl._
    
    implicit val cfg = TransformerConfiguration.default.enableMacrosLogging
    ```

The flag is also available to `Patcher`s, this code:

!!! example

    ```scala
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
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
    + Deriving Patcher expression for User with patch UserUpdateForm
      + Deriving Total Transformer expression from java.lang.String to Email
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
          + Deriving Total Transformer expression from java.lang.String to java.lang.String
            + Attempting expansion of rule Implicit
            + Rule Implicit decided to pass on to the next rule
            + Attempting expansion of rule Subtypes
            + Rule Subtypes expanded successfully: userupdateform.email
          + Derived recursively total expression userupdateform.email
        + Rule TypeToValueClass expanded successfully: new Email(userupdateform.email)
      + Deriving Total Transformer expression from scala.Long to Phone
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
          + Deriving Total Transformer expression from scala.Long to scala.Long
            + Attempting expansion of rule Implicit
            + Rule Implicit decided to pass on to the next rule
            + Attempting expansion of rule Subtypes
            + Rule Subtypes expanded successfully: userupdateform.phone
          + Derived recursively total expression userupdateform.phone
        + Rule TypeToValueClass expanded successfully: new Phone(userupdateform.phone)
    + Derived final expression is:
    | {
    |   val user: User = new User(user.id, new Email(userupdateform.email), new Phone(userupdateform.phone));
    |   user
    | }
    + Derivation took 0.064756000 s
    ```

## Ideas, questions or bug reports

If you haven't found a solution to your question try asking
at [GitHub discussions](https://github.com/scalalandio/chimney/discussions) page. If your idea looks like a
feature request or a bug, open an issue at [GitHub issues](https://github.com/scalalandio/chimney/issues) page.
