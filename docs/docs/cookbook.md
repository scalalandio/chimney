# Cookbook

Examples of various use cases already handled by Chimney.

## Reusing flags for several transformations/patchings

If we do not want to enable the same flag(s) in several places, we can define shared flag configuration as an implicit:

!!! example

    Scala 2

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    implicit val transformerCfg = TransformerConfiguration.default.enableMethodAccessors.enableMacrosLogging
    
    implicit val patcherCfg = PatcherConfiguration.default.ignoreNoneInPatch.enableMacrosLogging
    ```  

    Scala 3

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    transparent inline given TransformerConfiguration[?] =
      TransformerConfiguration.default.enableMethodAccessors.enableMacrosLogging
    
    transparent inline given PatcherConfiguration[?] =
      PatcherConfiguration.ignoreNoneInPatch.enableMacrosLogging
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
    implicit val foo2bar: Transformer[Foo, Bar] = ...
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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

## Java collections integration

If you need support for:

  - `java.util.Optional` and convert to/from it as if it was `scala.Option`
  - `java.util.Collection`/`java.lang.Iterable`/`java.util.Enumerable` and convert to/from it as if it was
    `scala.collection.IterableOnce` with a dedicated `Factory` (or `CanBuildFrom`)
  - `java.util.Map`/`java.util.Dictionary`/`java.util.Properties` and convert to/from `scala.collection.Map`
  - `java.util.stream`s and convert them to/from all sorts of Scala collections

Then you can use one simple import to enable it:

!!! example

    ```scala
    //> using dep io.scalaland::chimney-java-collections::{{ git.tag or local.tag }}
    import io.scalaland.chimney.javacollections._
    ```

!!! warning

    There is an important performance difference between Chimney conversion and `scala.jdk.converions`.
    
    While `asJava` and `asScala` attempt to be O(1) operations, by creating a cheap wrapper around the original
    collection, Chimney creates a full copy. It is the only way to
    
      - target various specific implementations of the target type
      - guarantee that you don't merely wrap a mutable type which could be mutated right after you wrap it 

## Cats integration

Cats integration module contains the following stuff:

  - type classes instances for partial transformers data structures
    - `Applicative` instance for `partial.Result`
    - `Semigroup` instance for `partial.Result.Errors`
  - integration with `Validated` (and `ValidatedNel`, `ValidatedNec`) data type for `PartialTransformer`s

!!! important

    You need to import ``io.scalaland.chimney.cats._`` to have all of the above in scope.

!!! example

    ```scala
    //> using dep io.scalaland::chimney-cats::{{ git.tag or local.tag }}
    
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
    import io.scalaland.chimney.cats._

    def hashpw(pw: String): String = "trust me bro, $pw is hashed"

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
version's parser didn't need it.

The automatic conversion into a protobuf with such a field can be problematic:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    object domain {
      case class Address(line1: String, line2: String)
    }
    object protobuf {
      case class Address(
        line1: String = "",
        line2: String = "",
        unknownFields: UnknownFieldSet = UnknownFieldSet()
      )
    }
  
    domain.Address("a", "b").transformInto[protobuf.Address]
    // error: Chimney can't derive transformation from domain.Address to protobuf.Address
    //
    // protobuf.Address
    //   unknownFields: UnknownFieldSet - no accessor named unknownFields in source type domain.Address
    //
    //
    // Consult https://scalalandio.github.io/chimney for usage examples.
    ```

There are 2 ways in which Chimney could handle this issue:

  - using [default values](supported-transformations.md#allowing-constructors-defaults)
  
    !!! example
  
        ```scala
        //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
        import io.scalaland.chimney.dsl._

         domain.Address("a", "b").into[protobuf.Address]
           .enableDefaultValues
           .transform
        ```

  - manually [setting this one field](supported-transformations.md#wiring-constructors-parameter-to-raw-value)_

    !!! example

        ```scala
        //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
        import io.scalaland.chimney.dsl._
    
        domain.Address("a", "b").into[protobuf.Address]
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
    
    object AddressBookType
        extends scalapb.GeneratedMessageCompanion[AddressBookType] {
      sealed trait Value extends scalapb.GeneratedOneof
      object Value {
        case object Empty extends AddressBookType.Value {
          // ...
        }
        final case class Public(value: AddressBookType.Public)
            extends AddressBookType.Value {
          // ...
        }
        final case class Private(value: AddressBookType.Private)
            extends AddressBookType.Value {
          // ...
        }
      }
      final case class Public(
      ) extends scalapb.GeneratedMessage
          with scalapb.lenses.Updatable[Public] {
      }
    
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
        //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
        import io.scalaland.chimney.dsl._
  
        pbType.value
          .intoPartial[addressbook.AddressBookType]
          .withCoproductInstancePartial[pb.addressbook.AddressBookType.Value.Empty.type](
            _ => partial.Result.fromEmpty
          )
          .transform
          .asOption == Some(domainType)
        ```

  - or handle all such fields with a single import:

    !!! example
  
        ```scala
        //> using dep io.scalaland::chimney-protobufs::{{ git.tag or local.tag }}
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    val domainStatus: order.CustomerStatus = order.CustomerStatus.CustomerRegistered
    val pbStatus: pb.order.CustomerStatus = pb.order.CustomerRegistered()
    
    domainStatus.into[pb.order.CustomerStatus].transform == pbStatus
    
    pbStatus
      .intoPartial[order.CustomerStatus]
      .withCoproductInstancePartial[pb.order.CustomerStatus.Empty.type](
        _ => partial.Result.fromEmpty
      )
      .withCoproductInstance[pb.order.CustomerStatus.NonEmpty](
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
    //> using dep io.scalaland::chimney-protobufs::{{ git.tag or local.tag }}
    import io.scalaland.chimney.protobufs._
    ```

## Libraries with smart constructors

Any type that uses a smart constructor (returning parsed result rather than throwing an exception) would require
Partial Transformer rather than Total Transformer to convert.

If there is no common interface that could be summoned as implicit for performing smart construction:

!!! example

    Assuming Scala 3 or `-Xsource:3` for fixed `private` constructors so that `Username.apply` and `.copy` would
    be private.

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Xsource:3
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.partial
  
    implicit def smartConstructedPartial[From, To](
      implicit smartConstructor: SmartConstructor[From, To]
    ): PartialTransformer[From, To] =
       PartialTransformer[From, To] { value =>
         partial.Result.fromEitherString(smartConstructor.parse(value))
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.estatico.newtype.Coercible
    import io.scalaland.chimney.Transformer
    
    implicit def newTypeTransformer[From, To](
        implicit coercible: Coercible[From, To]
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.{PartialTransformer, Transformer}
    import io.scalaland.chimney.partial
    import monix.newtypes._
    
    implicit def unwrapNewType[Outer, Inner](
        implicit extractor: HasExtractor.Aux[Outer, Inner]
    ): Transformer[Outer, Inner] = extractor.extract(_)
    
    implicit def wrapNewType[Inner, Outer](
        implicit builder: HasBuilder.Aux[Inner, Outer]
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
    //> using dep eu.timepit::refined::0.11.0
    import eu.timepit.refined._
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.auto._
    import eu.timepit.refined.collections._
    
    type Username = String Refined NonEmpty
    ```

We can validate using the dedicated type class (`Validate`), while extraction is a simple accessor:

!!! example

    ```scala
    //> using dep eu.timepit::refined::0.11.0
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import eu.timepit.refined.api.{Refined, Validate}
    import io.scalaland.chimney.{PartialTransformer, Transformer}
    import io.scalaland.chimney.partial
    
    implicit def extractRefined[Type, Refinement]:
        Transformer[Type Refined Refinement, Type] =
      _.value
    
    implicit def validateRefined[Type, Refinement](
        implicit validate: Validate.Plain[Type, Refinement]
    ): PartialTransformer[Type, Type Refined Refinement] =
      PartialTransformer[Type, Type Refined Refinement] { value =>
        partial.Result.fromOption(
          validate.validate(value).fold(Some(_), _ => None)
        )
      }
    ```

## Custom optional types

In case your library/domain defines custom Optional types, you can provide your own handling of such types through
implicits. Implicits we suggest you consider for implementation are:

  - non-optional type into optional-type of your option (`Transformer`)
  - optional-type into another optional-type of your option (`Transformer`)
  - optional-type into non-optional-type of your type (`PartialTransformer`)
  - optional-type of your option into `scala.Option` (`Transformer`)
  - `scala.Option` into optional-type of your option (`Transformer`)

It could look like this:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney._
    
    sealed trait MyOptional[+A]
    object MyOptional {
      case class Present[+A](value: A) extends MyOptional[A]
      case object Absent extends MyOptional[Nothing]
    }
    
    implicit def nonOptionalToOptional[A, B](implicit aToB: Transformer[A, B]):
        Transformer[A, MyOptional[B]] =
      a => MyOptional.Present(aToB.transform(a))
      
    implicit def optionalToOptional[A, B](implicit aToB: Transformer[A, B]):
        Transformer[MyOptional[A], MyOptional[B]] = {
      case MyOptional.Present(a) => MyOptional.Present(aToB.transform(a))
      case MyOptional.Absent     => MyOptional.Absent
    }
      
    implicit def optionalToNonOptional[A, B](implicit aToB: Transformer[A, B]):
        PartialTransformer[MyOptional[A], B] = PartialTransformer {
      case MyOptional.Present(a) => partial.Result.fromValue(aToB.transform(a))
      case MyOptional.Absent     => partial.Result.fromEmpty
    }
    
    implicit def optionalToOption[A, B](implicit aToB: Transformer[A, B]):
        Transformer[MyOptional[A], Option[B]] = {
      case MyOptional.Present(a) => Some(aToB.transform(a))
      case MyOptional.Absent     => None
    }
    
    implicit def optionToOptional[A, B](implicit aToB: Transformer[A, B]):
        Transformer[Option[A], MyOptional[B]] = {
      case Some(a) => MyOptional.Present(aToB.transform(a))
      case None    => MyOptional.Absent
    }
    ```
    
These 5 implicits are the bare minimum to make sure that:

  - your types will be automatically wrapped (always) and unwrapped (only with `PartialTransformer`s which can do it
    safely)
  - you can convert all kinds of values wrapped in your optional type
  - you can convert to and from `scala.Option`

An example of this approach can be seen in
[Java collections integration implementation](https://github.com/scalalandio/chimney/tree/master/chimney-java-collections/src/main/scala/io/scalaland/chimney/javacollections)
where it was used to provide support for `java.util.Optional`.  
