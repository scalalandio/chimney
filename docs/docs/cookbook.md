# Cookbook

## Using `PartialTransformer`s to their fullest potential

TODO

## Automatic, Semiautomatic and Inlined derivation

When you use the standard way of working with Chimney, but `import io.scalaland.chimney.dsl._`
you might notice that it is very convenient approach, making a lot of things easy:

  - when you want to trivially convert `val from: From` into `To` you can do
    it with `from.transformInto[To]`
  - the code above would be able to map case classes recursively
  - if you wanted to provide some transformation to use either directly in this
    `.transformInto` or in some nesting, you can do it just by using implicits
  - if you wanted to generate this implicit you could use `Transformer.derive`
  - if you needed to customize the derivation you could us
    `Transformer.define.customisationMethod.buildTransformer` or
    `from.into[To].customisationMethod.transform`

However, sometime you may want to restrict this behavior. It might be too easy to:

  - derive the same transformation again and again
  - define some customized `Transformer`, not import it by accident and still
    end up with compiling code since Chimney could derive a new one on the spot

### Automatic vs semiautomatic

In other libraries this issue is addressed by providing 2 flavours of derivation:

  - automatic derivation: usually requires some `import library.auto._`, allows you
    to get a derived instance just by summoning it e.g. with `implicitly[TypeClass[A]]`
    or calling any other method which would take it as `implicit` parameter.

    Usually, it is convenient to use, but has a downside of re-deriving the same instance
    each time you need it. Additionally, you cannot write

    !!! example
      
        ```scala
        implicit val typeclass: TypeClass[A] = implicitly[TypeClass[A]]
        ```

    since that generates circular dependency on a value initialisation. This makes it hard
    to cache this instance in e.g. companion object. In some libraries it also makes it hard
    to use automatic derivation to work with recursive data structures.

  - semiautomatic derivation: require you to explicitly call some method which will provide
    a derived instance. It has the downside that each instance that you would like to summon
    you need to manually derive and assign to an `implicit val` or `def`

    !!! example

        ```scala
        implicit val typeclass: TypeClass[A] = deriveTypeClass[A]
        ```

    However, it gives you certainty that each time you need an instance of a type class
    it will be the one you manually created. It reduces compile time, and make it easier
    to limit the places where error can happen (if you reuse the same instance everywhere
    and there is a bug in an instance, there is only one place to look for it).

The last property is a reason many projects encourage usage of semiautomatic derivation
and many libraries provide automatic derivation as quick and dirty way of doing things
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
        // defaults only
        Transformer.derive[From, To]
        PartialTransformer.derive[From, To]
        Patcher.derive[A, Patch]
        // allow customisation
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

    At a first glance, all they do is generate a customized type class before calling it, but
    what actually happens is that it generates an inlined expression, with no type class
    instantiation - if user provided type class for top-level or nested transformation it
    will be used, but wherever Chimney have to generate code ad hoc, it will generate inlined
    code. For that reason this could be considered a third mode, one where generated code
    is non-reusable, but optimized to avoid any type class allocation and deferring
    `partial.Result` wrapping (in case of `PartialTransformer` s) as long as possible.

### Performance concerns

When Chimney derives an expression, whether that is an expression directly inlined at call site
or as body of the `transform`/`patch` method inside a type class instance, it attempts
to generate a fast code.

It contains a special cases for `Option` s, `Either` s, it attempt to avoid boxing with
`partial.Result` and creating type classes if it can help it.

You can use [`.enableMacrosLogging`](troubleshooting.md#debugging-macros) to see the code generated by

!!! example

    ```scala
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

However, Chimney is only able to do it when given a free rein. It checks
if user provided an implicit, and if they did, it should be used instead.

In case of the automatic derivation, it means that every single branching
in the code - derivation for a field of a case class, or a subtype of a
sealed hierarchy - will trigger a macro, which may or mey not succeed
and it it will succeed it will introduce an allocation.

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
  - if you use unit test to ensure that you code does what it should and benchmarks to
    ensure it is reasonably fast keep on using `import dsl._`


## Java collections integration

TODO

## Cats integration

Cats integration module contains the following stuff:

  - type classes instances for partial transformers data structures
    - `Applicative` instance for `partial.Result`
    - `Semigroup` instance for `partial.Result.Errors`
  - integration with `Validated` (and `ValidatedNel`, `ValidatedNec`) data type for `PartialTransformer`s

!!! important

    You need to import ``io.scalaland.chimney.cats._`` in order to have all the above in scope.

!!! example

    ```scala
    case class RegistrationForm(email: String,
                                username: String,
                                password: String,
                                age: String)
  
    case class RegisteredUser(email: String,
                              username: String,
                              passwordHash: String,
                              age: Int)
  
    import io.scalaland.chimney._
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.cats._
    import cats.data._
  
    def validateEmail(form: RegistrationForm): ValidatedNec[String, String] = {
      if(form.email.contains('@')) {
        Validated.valid(form.email)
      } else {
        Validated.invalid(NonEmptyChain(s"${form.username}'s email: does not contain '@' character"))
      }
    }
  
    def validateAge(form: RegistrationForm): ValidatedNec[String, Int] = form.age.toIntOption match {
      case Some(value) if value >= 18 => Validated.valid(value)
      case Some(value) => Validated.invalid(NonEmptyChain(s"${form.username}'s age: must have at least 18 years"))
      case None => Validated.invalid(NonEmptyChain(s"${form.username}'s age: invalid number"))
    }
  
    implicit val partialTransformer: PartialTransformer[RegistrationForm, RegisteredUser] =
      PartialTransformer
        .define[RegistrationForm, RegisteredUser]
        .withFieldComputedPartial(_.email, form => validateEmail(form).toPartialResult)
        .withFieldComputed(_.passwordHash, form => hashpw(form.password))
        .withFieldComputedPartial(_.age, form => validateAge(form).toPartialResult)
        .buildTransformer
  
    val okForm = RegistrationForm("john@example.com", "John", "s3cr3t", "40")
    okForm.transformIntoPartial[RegisteredUser].asValidatedNec
    // Valid(RegisteredUser(email = "john@example.com", username = "John", passwordHash = "...", age = 40))
  
    Array(
      RegistrationForm("john_example.com", "John", "s3cr3t", "10"),
      RegistrationForm("alice@example.com", "Alice", "s3cr3t", "19"),
      RegistrationForm("bob@example.com", "Bob", "s3cr3t", "21.5")
    ).transformIntoPartial[Array[RegisteredUser]].asValidatedNel
    // Invalid(NonEmptyList(
    //   Error(StringMessage("John's email: does not contain '@' character"), ErrorPath(List(Index(0), Accessor("email")))),
    //   Error(StringMessage("John's age: must have at least 18 years"), ErrorPath(List(Index(0), Accessor("age")))),
    //   Error(StringMessage("Bob's age: invalid number"), ErrorPath(List(Index(2), Accessor("age"))))
    // ))
    ```

    Form validation logic is implemented in terms of `Validated` data type. You can easily convert
    it to a `partial.Result` required by `withFieldComputedPartial` by just using `.toPartialResult`
    which is available after importing the cats integration utilities (`import io.scalaland.chimney.cats._`).
    
    Result of the partial transformation is then converted to `ValidatedNel` or `ValidatedNec` using either
    `.asValidatedNel` or `.asValidatedNec` extension method call.

## Protocol Buffers integration

TODO

## Custom smart constructors

TODO

## Custom optional types

TODO
