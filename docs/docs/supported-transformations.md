# Supported Transformations

Chimney goes an extra mile to provide you with many reasonable transformations out of the box. Only if it isn't obvious
from the types, you need to provide it with a hint, but nothing more.

!!! note

    For your convenience, all examples will be presented as snippets runnable from
    [Scala CLI](https://scala-cli.virtuslab.org/). You can copy the content, paste it into a new `.sc` file,
    and compile it by running a command in the file's folder:

    ```bash
    # scala_version - e.g. {{ scala.2_12 }}, {{ scala.2_13 }} or {{ scala.3 }}
    # platform      - e.g. jvm, scala-js or scala-native
    scala-cli run --scala $scala_version --platform $platform .
    ```

## Total `Transformer`s vs `PartialTransformer`s

While looking at code examples you're going to see these 2 terms: **Total Transformers** and **Partial Transformers**.

Chimney's job is to generate the code that will convert the value of one type (often called a **source** type, or `From`)
into another type (often called a **target** type, or `To`). When Chimney has enough information to generate
the transformation, most of the time it could do it for **every** value of the source type. In Chimney, we called such
transformations Total (because they are virtually **total functions**). One way in which Chimney allows you to use such
transformation is through `Transformer[From, To]`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.Transformer

    class MyType(val a: Int)
    class MyOtherType(val b: String) {
      override def toString: String = s"MyOtherType($b)"
    }

    // There are better ways of defining implicit Transformer - see Transformer.derive[From, To] and
    // Transformer.define[From, To].buildTransformer - but for completely arbitrary type it's ok:
    val transformer: Transformer[MyType, MyOtherType] = (src: MyType) => new MyOtherType(src.a.toString)

    transformer.transform(new MyType(10)) // new MyOtherType("10")

    import io.scalaland.chimney.dsl._

    // When the compiler can find an implicit Transformer...
    implicit val transformerAsImplicit: Transformer[MyType, MyOtherType] = transformer

    // ...we can use this extension method to call it
    pprint.pprintln(
      (new MyType(10)).transformInto[MyOtherType]
    )
    // expected output:
    // MyOtherType(10)
    ```

For many cases, Chimney can generate this `Transformer` for you, without you having to do anything. As a matter of
fact, the majority of this page describes exactly that. In some cases Chimney might not know how to generate a total
transformation - but you would know, and you [could provide it yourself](#custom-transformations). But what if
converting one type into another cannot be described with a total function?    

Partial Transformers owe their name to **partial functions**. They might successfully convert only some values of
the source type. However, contrary to Scala's `PartialFunction` they do not throw an `Exception` when you pass a "wrong"
input into it. Instead, they return `partial.Result[To]`, which can store both successful and failed conversions and,
in case of failure, give you some information about the cause (an error message, an exception, value for which partial
function was not defined, "empty value" when something was expected) and even the path to the failed conversion
(a field name, an index of a collection, a key of a map).

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.{partial, PartialTransformer}

    class MyType(val b: String)
    class MyOtherType(val a: Int) {
      override def toString: String = s"MyOtherType($a)"
    }

    // There are better ways of defining implicit PartialTransformer - see PartialTransformer.derive[From, To] and
    // PartialTransformer.define[From, To].buildTransformer - but for completely arbitrary type it's ok
    val transformer: PartialTransformer[MyType, MyOtherType] =
      PartialTransformer[MyType, MyOtherType] { (src: MyType) =>
        partial.Result
         .fromCatching(src.b.toInt)
         .prependErrorPath(partial.PathElement.Accessor("b"))
         .map(a => new MyOtherType(a))
      }

    pprint.pprintln(
      transformer.transform(new MyType("10")).asEither.left.map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = MyOtherType(10))
    pprint.pprintln(
      transformer
        .transform(new MyType("NaN"))
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Left(
    //   value = List(
    //     ("b", ThrowableMessage(throwable = java.lang.NumberFormatException: For input string: "NaN"))
    //   )
    // )

    import io.scalaland.chimney.dsl._

    // When the compiler can find an implicit Transformer...:
    implicit val transformerAsImplicit: PartialTransformer[MyType, MyOtherType] = transformer

    // ...we can use this extension method to call it:
    pprint.pprintln(
      (new MyType("10"))
        .transformIntoPartial[MyOtherType]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = MyOtherType(10))

    pprint.pprintln(
      (new MyType("NaN"))
        .transformIntoPartial[MyOtherType]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Left(
    //   value = List(
    //     ("b", ThrowableMessage(throwable = java.lang.NumberFormatException: For input string: "NaN"))
    //   )
    // )
    ```

As you can see `partial.Result` contains `Iterable` as a structure for holding its errors. Thanks to that:

  - errors are aggregated - after partial transformation you have access to all failures that happened so that you
    could fix them at once, rather than rerunning the transformation several times
    - you can turn this off with a runtime flag, just call `.transformIntoPartial[To](failFast = true)`
  - errors are lazy - if their computation is expensive and they aren't used, you are not paying for it
  - there are some build-in conversions from `partial.Result` (e.g. to `Option` or `Either`), and there are
    [conversions to Cats types](cookbook.md#cats-integration), but you encouraged to convert them yourself
    to whatever data format you use to represents errors

!!! tip

    If you are wondering whether your case is for `Transformer` or `PartialTransformer` you can use the following rule
    of thumb:
    
      - quite often Chimney is used to convert to and from the domain model. The other side of the transformation might
        be a model generated by Protocol Buffers, an ADT used to generate JSON codecs, a model used to read/write
        from the database, etc.
      - then you almost always can convert a domain model into a DTO model - you can use the `Transformer` and consider
        it a **domain encoder** of sort
      - however when converting into the domain model you might guard against different invalid inputs: empty `Option`s,
        empty `String`s, values out of range - in such cases even if you wanted to be able to fail at one field, nested
        somewhere in ADT, Partial Transformers can be considered **domain decoders**
    
    In both of these cases, you might need to provide transformations for some types, buried in deep nesting, but often
    Chimney would be able to use them to generate a conversion of a whole model.

### `partial.Result` utilities

When you want to create a `PartialTransformer` you might need to create a `partial.Result`. In some cases you can get
away with just providing a throwing function, and letting some utility catch the `Exception`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.dsl._

    val fn: String => Int = str => str.toInt // Throws Exception if String is not a number.

    implicit val transformer: PartialTransformer[String, Int] =
      PartialTransformer.fromFunction(fn) // Catches exception!

    pprint.pprintln(
      "1".transformIntoPartial[Int].asEitherErrorPathMessageStrings
    )
    // expected output:
    // Right(value = 1)
    
    pprint.pprintln(
      "error".transformIntoPartial[Int].asEitherErrorPathMessageStrings
    )
    // expected output:
    // Left(value = List(("", "For input string: \"error\"")))
    ```

Other times you might need to convert `PartialFunction` into total function with `partial.Result`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    val fn: PartialFunction[String, Int] = {
      case str if str.forall(_.isDigit) => str.toInt
    }

    implicit val transformer: PartialTransformer[String, Int] =
      PartialTransformer(partial.Result.fromPartialFunction(fn)) // Handled "not defined at" case!

    pprint.pprintln(
      "1".transformIntoPartial[Int].asEitherErrorPathMessageStrings
    )
    // expected output:
    // Right(value = 1)
    
    pprint.pprintln(
      "error value".transformIntoPartial[Int].asEitherErrorPathMessageStrings
    )
    // expected output:
    // Left(value = List(("", "not defined at error value")))
    ```

However, the most common case would be where you would have to use one of utilities provided in `partial.Result`:

!!! example
 
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    implicit val transformer: PartialTransformer[String, Int] = PartialTransformer[String, Int] { str =>
      str match {
        case ""      => partial.Result.fromEmpty
        case "msg"   => partial.Result.fromErrorString("error message")
        case "error" => partial.Result.fromErrorThrowable(new Throwable("an error happened"))
        case value   => partial.Result.fromCatching(value.toInt)
      }
    }

    pprint.pprintln(
      List("", "error", "msg", "invaid").transformIntoPartial[List[Int]].asEitherErrorPathMessageStrings
    )
    // expected output:
    // Left(
    //   value = List(
    //     ("(0)", "empty value"),
    //     ("(1)", "an error happened"),
    //     ("(2)", "error message"),
    //     ("(3)", "For input string: \"invaid\"")
    //   )
    // )
    ```

If you are converting from: `Option`s, `Either[String, A]` or `Try` you can use an extension method:

!!! example
 
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.partial.syntax._

    pprint.pprintln(
      (Some(1): Option[Int]).asResult.asEitherErrorPathMessageStrings
    )
    pprint.pprintln(
      (None: Option[Int]).asResult.asEitherErrorPathMessageStrings
    )
    // expected output:
    // Right(value = 1)
    // Left(value = List(("", "empty value")))

    pprint.pprintln(
      (Right(1): Either[String, Int]).asResult.asEitherErrorPathMessageStrings
    )
    pprint.pprintln(
      (Left("invalid"): Either[String, Int]).asResult.asEitherErrorPathMessageStrings
    )
    // expected output:
    // Right(value = 1)
    // Left(value = List(("", "invalid")))

    import scala.util.Try
    pprint.pprintln(
      Try("1".toInt).asResult.asEitherErrorPathMessageStrings
    )
    pprint.pprintln(
      Try("invalid".toInt).asResult.asEitherErrorPathMessageStrings
    )
    // expected output:
    // Right(value = 1)
    // Left(value = List(("", "For input string: \"invalid\"")))
    ```

## Upcasting and identity transformation

If you transform one type into itself or its supertype, it will be upcast without any change.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    trait A
    class B extends A
    val b = new B
    b.transformInto[A] // == (b: A)
    b.into[A].transform // == (b: A)
    b.transformIntoPartial[A].asEither // == Right(b: A)
    b.intoPartial[A].transform.asEither // == Right(b: A)

    import io.scalaland.chimney.{Transformer, PartialTransformer}

    // If we want to reuse Transformer, we can create implicits using:
    val totalTransformer: Transformer[A, B] = Transformer.derive[A, B]
    val partialTransformer: PartialTransformer[A, B] = PartialTransformer.derive[A, B]
    // or (if you want to pass overrides):
    val totalTransformer2: Transformer[A, B] = Transformer.define[A, B]
      .buildTransformer
    val partialTransformer2: PartialTransformer[A, B] = PartialTransformer.define[A, B]
      .buildTransformer
    ```

In particular, when the source type is (`=:=`) the target type, you will end up with an identity transformation.

!!! warning

    Checking if value can be upcast is the second thing Chimney attempts (right after
    [looking for an implicit](#custom-transformations)).
    
    This attempt is only skipped if we customised the transformation:
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class A(val a: String)
    class B extends A("value")
    val b = new B

    b.into[A].withFieldConst(_.a, "copied").transform // new A("copied")

    import io.scalaland.chimney.Transformer
    Transformer.define[B, A].withFieldConst(_.a, "copied").buildTransformer.transform(b) // new A("copied")
    ```
    
    since that customization couldn't be applied if we only upcasted the value. 

### Type-evidence-based conversions

But default conversions using `=:=` and `<:<` are disabled, but they can be enabled with a flag
`.enableTypeConstraintEvidence`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo[A](value: A)
    case class Bar[A](value: A)

    def fooToBar[A, B](value: Foo[A])(implicit ev: A <:< B): Bar[B] =
        value.into[Bar[B]].enableTypeConstraintEvidence.transform
  
    pprint.pprintln(
       fooToBar[String, String](Foo("bar"))
    )
    // expected output:
    // Bar(value = "bar")

    def fooToBar2[A, B](value: Foo[A])(implicit ev: A <:< B): Bar[B] = {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableTypeConstraintEvidence

      value.transformInto[Bar[B]]
    }
  
    pprint.pprintln(
      fooToBar2[String, String](Foo("bar"))
    )
    // expected output:
    // Bar(value = "bar")
    ```

If the flag was enabled in the implicit config it can be disabled with `.enableTypeConstraintEvidence`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo[A](value: A)
    case class Bar[A](value: A)

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableTypeConstraintEvidence

    def fooToBar[A, B](value: Foo[A])(implicit ev: A <:< B): Bar[B] =
      value.into[Bar[B]].disableTypeConstraintEvidence.transform
  
    pprint.pprintln(
       fooToBar[String, String](Foo("bar"))
    )
    // expected error:
    // Chimney can't derive transformation from Foo[A] to Bar[B]
    // 
    // Bar[B]
    //   value: B - can't derive transformation from value: A in source type Foo[A]
    // 
    // B (transforming from: value into: value)
    //   derivation from foo.value: A to B is not supported in Chimney!
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

## Into a `case class` (or POJO)

Every type can have its `val`s read and used as data sources for the transformation.

And every class with a public primary constructor can be the target of the transformation. (Or, if the primary is not
public, with exactly one public constructor to make the choice unambiguous).

To make it work out of the box, every argument of a constructor needs to be paired with a matching field (`val`) in the
transformed value.

!!! tip

    The intuition is that we are matching fields of a source `case class` with fields of a target `case class` by their
    name. And if for every field in the target `case class` there is a field of the same name in the source, we will use
    it.
    
    However, Chimney is **not** limited to `case class`-to-`case class` mappings and you can target **every** class (with
    a public constructor) as if it was a `case class` and read every `val` from source as if it was a case class field.

The obvious examples are `case class`es with the same fields:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Source(a: Int, b: Double)
    case class Target(a: Int, b: Double)

    pprint.pprintln(
      Source(42, 0.07).transformInto[Target]
    )
    pprint.pprintln(
      Source(42, 0.07).into[Target].transform
    )
    pprint.pprintln(
      Source(42, 0.07).transformIntoPartial[Target].asEither
    )
    pprint.pprintln(
      Source(42, 0.07).intoPartial[Target].transform.asEither
    )
    // expected output:
    // Target(a = 42, b = 0.07)
    // Target(a = 42, b = 0.07)
    // Right(value = Target(a = 42, b = 0.07))
    // Right(value = Target(a = 42, b = 0.07))

    import io.scalaland.chimney.{Transformer, PartialTransformer}

    // If we want to reuse Transformer, we can create implicits using:
    val totalTransformer: Transformer[Source, Target] = Transformer.derive[Source, Target]
    val partialTransformer: PartialTransformer[Source, Target] = PartialTransformer.derive[Source, Target]
    // or (if you want to pass overrides):
    val totalTransformer2: Transformer[Source, Target] = Transformer.define[Source, Target]
      .buildTransformer
    val partialTransformer2: PartialTransformer[Source, Target] = PartialTransformer.define[Source, Target]
      .buildTransformer
    ```

However, the original value might have fields absent in the target type and/or appearing in a different order:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Source(a: Int, b: Double, c: String)
    case class Target(b: Double, a: Int)

    pprint.pprintln(
      Source(42, 0.07, "value").transformInto[Target]
    )
    pprint.pprintln(
      Source(42, 0.07, "value").into[Target].transform
    )
    pprint.pprintln(
      Source(42, 0.07, "value").transformIntoPartial[Target].asEither
    )
    pprint.pprintln(
      Source(42, 0.07, "value").intoPartial[Target].transform.asEither
    )
    // expected output:
    // Target(b = 0.07, a = 42)
    // Target(b = 0.07, a = 42)
    // Right(value = Target(b = 0.07, a = 42))
    // Right(value = Target(b = 0.07, a = 42))
    ```

It doesn't even have to be a `case class`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Source {
      val a: String = "a"
      val b: Int = 1024
    }
    class Target(a: String, b: Int)

    (new Source).transformInto[Target]
    // like:
    // val source = new Source
    // new Target(source.a, source.b)
    ```

nor have the same types of fields - as long as transformation for each pair (a field and a constructor's argument) can
be resolved recursively:

!!! example

    During conversion from `Foo` to `Bar` we are automatically converting `Foo.Baz` into `Bar.Baz` 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(baz: Foo.Baz)
    object Foo { case class Baz(baz: String) }

    case class Bar(baz: Bar.Baz)
    object Bar { case class Baz(baz: String) }

    pprint.pprintln(
      Foo(Foo.Baz("baz")).transformInto[Bar]
    )
    pprint.pprintln(
      Foo(Foo.Baz("baz")).into[Bar].transform
    )
    pprint.pprintln(
      Foo(Foo.Baz("baz")).transformIntoPartial[Bar].asEither
    )
    pprint.pprintln(
      Foo(Foo.Baz("baz")).intoPartial[Bar].transform.asEither
    )
    // expected output:
    // Bar(baz = Baz(baz = "baz"))
    // Bar(baz = Baz(baz = "baz"))
    // Right(value = Bar(baz = Baz(baz = "baz")))
    // Right(value = Bar(baz = Baz(baz = "baz")))
    ```

As we see, for infallible transformations there is very little difference in behavior between Total and Partial
Transformers. For "products" the difference shows up when transformation for any field/constructor fails. One such fallible
transformation, available only in partial transformers, is unwrapping `Option` fields.

!!! example

    Partial Transformers preserve the path (with nestings!) to the failed transformation

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(baz: Foo.Baz)
    object Foo { case class Baz(baz: Option[String]) }

    case class Bar(baz: Bar.Baz)
    object Bar { case class Baz(baz: String) }

    pprint.pprintln(
      Foo(Foo.Baz(Some("baz")))
        .transformIntoPartial[Bar]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    pprint.pprintln(
      Foo(Foo.Baz(None))
        .transformIntoPartial[Bar]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = Bar(baz = Baz(baz = "baz")))
    // Left(value = List(("baz.baz", EmptyValue)))
    ```

Examples so far assumed, that each constructor's argument was paired with a field of the same name. So, let's show what
to do if that isn't the case.

### Reading from methods

If we want to read from `def fieldName: A` as if it was `val fieldName: A` - which could be unsafe as it might perform
side effects - you need to enable the `.enableMethodAccessors` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Source(_a: String, _b: Int) {
      def a: String = _a
      def b(): Int = _b
    }
    class Target(a: String, b: Int)

    (new Source("value", 512))
      .into[Target]
      .enableMethodAccessors
      .transform
    // val source = new Source("value", 512)
    // new Target(source.a, source.b())
    (new Source("value", 512))
      .intoPartial[Target]
      .enableMethodAccessors
      .transform
    // val source = new Source("value", 512)
    // partial.Result.fromValue(new Target(source.a, source.b()))

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableMethodAccessors

      (new Source("value", 512)).transformInto[Target]
      // val source = new Source("value", 512)
      // new Target(source.a, source.b())
      (new Source("value", 512)).transformIntoPartial[Target]
      // val source = new Source("value", 512)
      // partial.Result.fromValue(new Target(source.a, source.b()))
    }

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Source, Target] = Transformer.define[Source, Target]
      .enableMethodAccessors
      .buildTransformer
    ```

Flag `.enableMethodAccessors` will allow macros to consider methods that are:

  - nullary (take 0 value arguments)
  - have no type parameters
  - cannot be considered Bean getters

If the flag was enabled in the implicit config it can be disabled with `.disableMethodAccessors`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Source(_a: String, _b: Int) {
      def a: String = _a
      def b(): Int = _b
    }
    class Target(a: String, b: Int)

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableMethodAccessors

    (new Source("value", 512)).into[Target].disableMethodAccessors.transform
    // expected error:
    // Chimney can't derive transformation from Source to Target
    //
    // Target
    //   a: java.lang.String - no accessor named a in source type Source
    //   b: scala.Int - no accessor named b in source type Source
    //
    // There are methods in Source that might be used as accessors for a (e.g. a), b (e.g. b), constructor arguments/setters in Target. Consider using .enableMethodAccessors.
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

### Reading from inherited values/methods

Out of the box, only values defined directly within the source type are considered. If we want to read from `val`s
inherited from a source value's supertype, you need to enable the `.enableInheritedAccessors` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    trait Parent {
      val a = "value"
    }
    case class Source(b: Int) extends Parent
    case class Target(a: String, b: Int)

    pprint.pprintln(
      Source(10).into[Target].enableInheritedAccessors.transform
    )
    pprint.pprintln(
      Source(10).intoPartial[Target].enableInheritedAccessors.transform.asEither
    )
    // expected output:
    // Target(a = "value", b = 10)
    // Right(value = Target(a = "value", b = 10))

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableInheritedAccessors

      pprint.pprintln(
        Source(10).transformInto[Target]
      )
      pprint.pprintln(
        Source(10).transformIntoPartial[Target].asEither
      )
      // expected output:
      // Target(a = "value", b = 10)
      // Right(value = Target(a = "value", b = 10))
    }

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Source, Target] = Transformer.define[Source, Target]
      .enableInheritedAccessors
      .buildTransformer
    ```

!!! tip

    `.enableInheritedAccessors` can be combined with [`.enableMethodAccessors`](#reading-from-inherited-valuesmethods)
    and [`.enableBeanGetters`](#reading-from-bean-getters) to allow reading from inherited `def`s and Bean getters.

If the flag was enabled in the implicit config it can be disabled with `.enableInheritedAccessors`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    trait Parent {
      val a = "value"
    }
    case class Source(b: Int) extends Parent
    case class Target(a: String, b: Int)

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableInheritedAccessors

    Source(10).into[Target].disableInheritedAccessors.transform
    // expected error:
    // Chimney can't derive transformation from Source to Target
    //
    // Target
    //   a: java.lang.String - no accessor named a in source type Source
    //
    // There are inherited definitions in Source that might be used as accessors for a (e.g. a), the constructor argument/setter in Target. Consider using .enableInheritedAccessors.
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

### Reading from Bean getters

If we want to read `def getFieldName(): A` as if it was `val fieldName: A` - which would allow reading from Java Beans
(or Plain Old Java Objects) - you need to enable a flag: 

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Source(a: String, b: Int) {
      def getA(): String = a
      def getB(): Int = b
    }
    class Target(a: String, b: Int)

    (new Source("value", 512))
      .into[Target]
      .enableBeanGetters
      .transform
    // val source = new Source("value", 512)
    // new Target(source.getA(), source.getB())
    (new Source("value", 512))
      .intoPartial[Target]
      .enableBeanGetters
      .transform
    // val source = new Source("value", 512)
    // partial.Result.fromValue(new Target(source.getA(), source.getB()))

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableBeanGetters

      (new Source("value", 512)).transformInto[Target]
      // val source = new Source("value", 512)
      // new Target(source.getA(), source.getB())
      (new Source("value", 512)).transformIntoPartial[Target]
      // val source = new Source("value", 512)
      // partial.Result.fromValue(new Target(source.getA(), source.getB()))
    }

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Source, Target] = Transformer.define[Source, Target]
      .enableBeanGetters
      .buildTransformer
    ```

Flag `.enableBeanGetters` will allow macros to consider methods which are:

  - nullary (take 0 value arguments)
  - have no type parameters
  - have names starting with `get` - for comparison `get` will be dropped and the first remaining letter lowercased or
  - have names starting with `is` and returning `Boolean` - for comparison `is` will be dropped and the first remaining
    letter lowercased

which would otherwise be ignored when analyzing possible sources of values.

If the flag was enabled in the implicit config it can be disabled with `.disableBeanGetters`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Source(a: String, b: Int) {
      def getA(): String = a
      def getB(): Int = b
    }
    class Target(a: String, b: Int)

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableBeanGetters

    (new Source("value", 512)).into[Target].disableBeanGetters.transform
    // expected error:
    // Chimney can't derive transformation from Source to Target
    //
    // Target
    //   a: java.lang.String - no accessor named a in source type Source
    //   b: scala.Int - no accessor named b in source type Source
    //
    // There are methods in Source that might be used as accessors for `a`, `b` fields in Target. Consider using `.enableMethodAccessors`.
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

### Writing to Bean setters

If we want to write to `def setFieldName(fieldName: A): Unit` as if it was `fieldName: A` argument of a constructor -
which would allow creating from Java Beans (or Plain Old Java Objects) - you need to enable the `.enableBeanSetters`
flag: 

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Source(val a: String, val b: Int)
    class Target() {
      private var a = ""
      def setA(a_ : String): Unit = a = a_
      private var b = 0
      def setB(b_ : Int): Unit = b = b_
    }

    (new Source("value", 512))
      .into[Target]
      .enableBeanSetters
      .transform
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.setA(source.a)
    // target.setB(source.b)
    // target
    (new Source("value", 512))
      .intoPartial[Target]
      .enableBeanSetters
      .transform
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.setA(source.a)
    // target.setB(source.b)
    // partial.Result.fromValue(target)

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableBeanSetters

      (new Source("value", 512)).transformInto[Target]
      // val source = new Source("value", 512)
      // val target = new Target()
      // target.setA(source.a)
      // target.setB(source.b)
      // target
      (new Source("value", 512)).transformIntoPartial[Target]
      // val source = new Source("value", 512)
      // val target = new Target()
      // target.setA(source.a)
      // target.setB(source.b)
      // partial.Result.fromValue(target)
    }

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Source, Target] = Transformer.define[Source, Target]
      .enableBeanSetters
      .buildTransformer
    ```

Flag `.enableBeanSetters` will allow macros to write to methods which are:

  - unary (take 1 value argument)
  - have no type parameters
  - have names starting with `set` - for comparison `set` will be dropped and the first remaining letter lowercased
  - returning `Unit` (this condition can be turned off)

_besides_ calling constructor (so you can pass values to _both_ the constructor and setters at once). Without the flag
macro will fail compilation to avoid creating potentially uninitialized objects.

!!! warning

    0.8.0 dropped the requirement that the setter needs to return `Unit`. It enabled targeting mutable builders, which
    let you chain calls with fluent API, but are still mutating the state internally, making this chaining optional.

    However, it broke code with unary, non-`Unit` `set*` methods that weren't intended to be used as setters, therfore
    1.0.0 changed it so that non-`Unit` setters are opt-in with `.enableNonUnitBeanSetters` flag.

If the flag was enabled in the implicit config it can be disabled with `.disableBeanSetters`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Source(val a: String, val b: Int)
    class Target() {
      private var a = ""
      def getA: String = a
      def setA(aa: String): Unit = a = aa
      private var b = 0
      def getB(): Int = b
      def setB(bb: Int): Unit = b = bb
    }

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableBeanSetters

    (new Source("value", 512)).into[Target].disableBeanSetters.transform
    // expected error:
    // Chimney can't derive transformation from Source to Target
    //
    // Target
    //   derivation from source: Source to Target is not supported in Chimney!
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

This flag would **also** enable writing to public `var`s:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Source(val a: String, val b: Int)
    class Target() {
      var a: String = ""
      var b: Int = 0
    }

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableBeanSetters

    (new Source("value", 512)).transformInto[Target]
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.a = source.a
    // target.b = source.b
    // target
    (new Source("value", 512)).transformIntoPartial[Target]
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.a = source.a
    // target.b = source.b
    // partial.Result.fromValue(target)
    ```

It's an unplanned but internally consistent outcome of several overlapping requirements:

 - Chimney tries to keep behavior consistent between Scala 2 and Scala 3
 - Chimney tries (best effort, no tests currently) to handle classes compiled with 2.13 in macros compiled with Scala 3
   and vice versa
 - Scala 3 [changed the behavior of `@BeanProperty`](https://docs.scala-lang.org/scala3/guides/migration/incompat-other-changes.html#invisible-bean-property)
   so that `var`'s would generate getters and setters visible only from Java - Scala would see only `var`s

So consistent behavior on Scala 3 requires aligning writing to `var`a with using setters, and Scala 2/Scala 3 parity
requires doing the same on Scala 2.

This allows using `.enableBeanSetters` to handle transformations of Scala.js' `js.Object`s:

!!! example

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using platform scala-js
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import scala.scalajs.js
    import scala.scalajs.js.annotation.JSGlobal
    import io.scalaland.chimney.dsl._
    
    @js.native
    @JSGlobal
    class RTCIceCandidate1 extends js.Object {
    
      /** Returns a transport address for the candidate that can be used for connectivity checks. The format of this address
        * is a candidate-attribute as defined in RFC 5245.
        */
      var candidate: String = js.native
    
      /** If not null, this contains the identifier of the "media stream identification" (as defined in RFC 5888) for the
        * media component this candidate is associated with.
        */
      var sdpMid: String = js.native
    
      /** If not null, this indicates the index (starting at zero) of the media description (as defined in RFC 4566) in the
        * SDP this candidate is associated with.
        */
      var sdpMLineIndex: Double = js.native
    }
    
    case class RTCIceCandidate2(candidate: String, sdpMid: String, sdpMLineIndex: Double)
    
    def convertBackAndForth: Unit = {
    val c1 = RTCIceCandidate2("test", "test", 2.0)
    
    val c2 = c1.into[RTCIceCandidate1].enableBeanSetters.transform
    
    val c3 = c2.transformInto[RTCIceCandidate2]
    }
    ```

### Ignoring unmatched Bean setters

If the target class has any method that Chimney recognized as a setter, by default it will refuse to generate the code
unless we explicitly tell it what to do with these setters. If using them is not what we intended, we can also ignore
them:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Target() {
      private var a = ""
      def getA: String = a
      def setA(aa: String): Unit = a = aa
      private var b = 0
      def getB(): Int = b
      def setB(bb: Int): Unit = b = bb
    }

    ().into[Target]
      .enableIgnoreUnmatchedBeanSetters
      .transform // new Target()
    ()
      .intoPartial[Target]
      .enableIgnoreUnmatchedBeanSetters
      .transform // partial.Result.fromValue(new Target())

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableIgnoreUnmatchedBeanSetters

      ().transformInto[Target] // new Target()
      ().transformIntoPartial[Target] // partial.Result.fromValue(new Target())
    }

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Unit, Target] = Transformer.define[Unit, Target]
      .enableIgnoreUnmatchedBeanSetters
      .buildTransformer
    ```

If the flag was enabled in the implicit config it can be disabled with `.disableIgnoreUnmatchedBeanSetters`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Target() {
      private var a = ""
      def getA: String = a
      def setA(aa: String): Unit = a = aa
      private var b = 0
      def getB(): Int = b
      def setB(bb: Int): Unit = b = bb
    }

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableIgnoreUnmatchedBeanSetters

    ().into[Target].disableIgnoreUnmatchedBeanSetters.transform
    // expected error:
    // Chimney can't derive transformation from scala.Unit to Target
    //
    // Target
    //   setA(a: java.lang.String) - no accessor named a in source type scala.Unit
    //   setB(b: scala.Int) - no accessor named b in source type scala.Unit
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

This flag can be combined with [`.enableBeanSetters`](#writing-to-bean-setters), so that:

  - setters will attempt to be matched with fields from source
  - setters could be overridden manually using `.withField*` methods
  - those setters which would have no matching fields nor overrides would just be ignored

making this setting sort of a setters' counterpart to a default value in a constructor.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Source(val a: String)
    class Target() {
      private var a = ""
      def setA(a_ : String): Unit = a = a_
      private var b = 0
      def setB(b_ : Int): Unit = b = b_
    }

    (new Source("value"))
      .into[Target]
      .enableBeanSetters
      .enableIgnoreUnmatchedBeanSetters
      .transform
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.setA(source.a)
    // target
    (new Source("value"))
      .intoPartial[Target]
      .enableBeanSetters
      .enableIgnoreUnmatchedBeanSetters
      .transform
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.setA(source.a)
    // partial.Result.fromValue(target)

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableBeanSetters.enableIgnoreUnmatchedBeanSetters

      (new Source("value")).transformInto[Target]
      // val source = new Source("value", 512)
      // val target = new Target()
      // target.setA(source.a)
      // target
      (new Source("value")).transformIntoPartial[Target]
      // val source = new Source("value", 512)
      // val target = new Target()
      // target.setA(source.a)
      // partial.Result.fromValue(target)
    }
    ```

It is disabled by default for the same reasons as default values - being potentially dangerous.

### Writing to non-`Unit` Bean setters

By default, only unary methods returning `Unit` and starting with `set*` are considered setters. But this would exclude
e.g. some builder methods which return `this.type` despite mutating. Such methods are silently ignored.

To consider such methods (and fail compilation if they are not matched) you can enable them with a flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Source(val a: String, val b: Int)
    class Target() {
      private var a: String = _
      private var b: Int = _

      def getA(): String = a
      def setA(a: String): Unit = this.a = a

      def getB(): Int = b
      def setB(b: Int): Target = {
        this.a = a
        this
      }
    }

    // setB is silently ignored:

    new Source("value", 128).into[Target].enableBeanSetters.transform
    // val source = new Source("value", 128)
    // val target = new Target()
    // target.setA(source.a)
    // target

    // setB is considered:

    new Source("value", 128).into[Target].enableBeanSetters.enableNonUnitBeanSetters.transform
    // val source = new Source("value", 128)
    // val target = new Target()
    // target.setA(source.a)
    // target.setB(source.b)
    // target

    new Source("value", 128).intoPartial[Target].enableBeanSetters.enableNonUnitBeanSetters.transform
    // val source = new Source("value", 128)
    // val target = new Target()
    // target.setA(source.a)
    // target.setB(source.b)
    // partial.Result.fromValue(target)

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableBeanSetters.enableNonUnitBeanSetters

      new Source("value", 128).transformInto[Target]
      // val source = new Source("value", 128)
      // val target = new Target()
      // target.setA(source.a)
      // target.setB(source.b)
      // target

      new Source("value", 128).transformIntoPartial[Target]
      // val source = new Source("value", 128)
      // val target = new Target()
      // target.setA(source.a)
      // target.setB(source.b)
      // partial.Result.fromValue(target)
    }

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Source, Target] = Transformer.define[Source, Target]
      .enableBeanSetters
      .enableNonUnitBeanSetters
      .buildTransformer
    ```

It is disabled by default for the same reasons as default values - being potentially dangerous.

### Fallback to `Unit` as the constructor's argument

If a class' constructor takes `Unit` as a parameter, it is always provided without any configuration.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Source()
    case class Target(value: Unit)

    pprint.pprintln(
      Source().transformInto[Target]
    )
    pprint.pprintln(
      Source().into[Target].transform
    )
    // expected output:
    // Target(value = ())
    // Target(value = ())
    pprint.pprintln(
      Source().transformIntoPartial[Target].asEither
    )
    pprint.pprintln(
      Source().intoPartial[Target].transform.asEither
    )
    // expected output:
    // Right(value = Target(value = ()))
    // Right(value = Target(value = ()))
    ```

### Fallback to literal-based singleton types as the constructor's argument

If a class' constructor takes literal-based singleton types as a parameter (e.g. due to type parameter application),
it is always provided without any configuration (on [Scala 2.13/3](https://docs.scala-lang.org/sips/42.type.html),
since 2.12 did not yet have this concept).

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Source()
    case class Target(value: "my value")

    pprint.pprintln(
      Source().transformInto[Target]
    )
    pprint.pprintln(
      Source().into[Target].transform
    )
    // expected output:
    // Target(value = "my value")
    // Target(value = "my value")
    pprint.pprintln(
      Source().transformIntoPartial[Target].asEither
    )
    pprint.pprintln(
      Source().intoPartial[Target].transform.asEither
    )
    // expected output:
    // Right(value = Target(value = "my value"))
    // Right(value = Target(value = "my value"))
    ```

### Fallback to case objects as the constructor's argument

If a class' constructor takes `case object` as a parameter (e.g. due to type parameter application), it is always
provided without any configuration (on [Scala 2.13/3](https://docs.scala-lang.org/sips/42.type.html), since 2.12 did not
yet have this concept).

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case object SomeObject

    case class Source()
    case class Target(value: SomeObject.type)

    pprint.pprintln(
      Source().transformInto[Target]
    )
    pprint.pprintln(
      Source().into[Target].transform
    )
    // expected output:
    // Target(value = SomeObject)
    // Target(value = SomeObject)
    pprint.pprintln(
      Source().transformIntoPartial[Target].asEither
    )
    pprint.pprintln(
      Source().intoPartial[Target].transform.asEither
    )
    // expected output:
    // Right(value = Target(value = SomeObject))
    // Right(value = Target(value = SomeObject))
    ```

On Scala 3, parameterless `case` can be used as well:    

!!! example

    ```scala
    // file: snippet.scala - part of example of parameterless case as fallback value
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    enum SomeEnum:
      case SomeValue

    case class Source()
    case class Target(value: SomeEnum.SomeValue.type)

    @main def example: Unit = {
    pprint.pprintln(
      Source().transformInto[Target]
    )
    pprint.pprintln(
      Source().into[Target].transform
    )
    // expected output:
    // Target(value = SomeValue)
    // Target(value = SomeValue)
    
    pprint.pprintln(
      Source().transformIntoPartial[Target].asEither
    )
    pprint.pprintln(
      Source().intoPartial[Target].transform.asEither
    )
    // expected output:
    // Right(value = Target(value = SomeValue))
    // Right(value = Target(value = SomeValue))
    }
    ```

!!! notice

    Only `case object`s and parameterless `case`s are supported this way - other `object`s, or singletons defined
    for `value.type` are not supported at the moment.
    
!!! notice

    `None.type` is explicitly excluded from this support as it might accidentally fill the value that should not be
    filled - provide it explicitly or enable with `.enableOptionDefaultsToNone`.

### Allowing fallback to the constructor's default values

When calling the constructor manually, sometimes we want to not pass all arguments ourselves and let the default values
handle the remaining ones. If Chimney did it out of the box, it could lead to some subtle bugs - you might prefer a
compilation error reminding you to provide the value yourself - but if you know that it is safe you can enable fallback
to default values with the `.enableDefaultValues` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Source(a: String, b: Int)
    case class Target(a: String, b: Int = 0, c: Long = 0L)

    pprint.pprintln(
      Source("value", 128).into[Target].enableDefaultValues.transform
      // val source = Source("value", 128)
      // Target(source.a, source.b /* c is filled by the default value */)
    )
    pprint.pprintln(
      Source("value", 128).intoPartial[Target].enableDefaultValues.transform
      // val source = Source("value", 128)
      // partial.Result.fromValue(Target(source.a, source.b /* c is filled by the default value */))
    )
    // expected output:
    // Target(a = "value", b = 128, c = 0L)
    // Value(value = Target(a = "value", b = 128, c = 0L))

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Source, Target] = Transformer.define[Source, Target]
      .enableDefaultValues
      .buildTransformer
    ```

A default value is used as a fallback, meaning:

  - it has to be defined (and enabled with a flag)
  - it will not be used if you provided value manually with one of the methods below - then the value provision always
    succeeds
  - it will not be used if a source field (`val`) or a method (enabled with one of the flags above) with a matching name
    could be found - if a source value type can be converted into a target argument/setter type then the value provision
    succeeds, but if Chimney fails to convert the value then the whole derivation fails rather than falls back to
    the default value 

If the flag was enabled in the implicit config it can be disabled with `.disableDefaultValues`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class Source(a: String, b: Int)
    case class Target(a: String, b: Int = 0, c: Long = 0L)

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableDefaultValues

    (new Source("value", 512)).into[Target].disableDefaultValues.transform
    // expected error:
    // Chimney can't derive transformation from Source to Target
    //
    // Target
    //   c: scala.Long - no accessor named c in source type Source
    //
    // There are default values for c, the constructor argument/setter in Target. Consider using .enableDefaultValues or .enableDefaultValueForType.
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

If enabling global values globally, seems too dangerous, you can also limit the scope of their usage, by enabling only
default values of one particular type:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Source(a: String, b: Int)
    case class Target(a: String, b: Int = 0, c: Long = 0L)

    pprint.pprintln(
      Source("value", 128).into[Target].enableDefaultValueOfType[Long].transform
      // val source = Source("value", 128)
      // Target(source.a, source.b /* c is filled by the default value */)
    )
    pprint.pprintln(
      Source("value", 128).intoPartial[Target].enableDefaultValueOfType[Long].transform
      // val source = Source("value", 128)
      // partial.Result.fromValue(Target(source.a, source.b /* c is filled by the default value */))
    )
    // expected output:
    // Target(a = "value", b = 128, c = 0L)
    // Value(value = Target(a = "value", b = 128, c = 0L))
    ```

### Allowing fallback to `None` as the constructor's argument

Sometimes we transform value into a type that would use `Option`'s `None` to handle some default behavior and
`Some` as the user's overrides. This type might not have a default value (e.g. `value: Option[A] = None`) in its
constructor, but we would find it useful to fall back on `None` in such cases. It is not enabled out of the box, for
similar reasons to default values support, but we can enable it with the `.enableOptionDefaultsToNone` flag:   

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String)
    case class Bar(a: String, b: Option[String] = Some("a"))

    // Without these flags -> compilation error!
    pprint.pprintln(
      Foo("value").into[Bar].enableOptionDefaultsToNone.transform
    )
    pprint.pprintln(
      Foo("value").intoPartial[Bar].enableOptionDefaultsToNone.transform.asOption
    )
    // expected ouput:
    // Bar(a = "value", b = None)
    // Some(value = Bar(a = "value", b = None))

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableOptionDefaultsToNone

      pprint.pprintln(
        Foo("value").transformInto[Bar]
      )
      pprint.pprintln(
        Foo("value").transformIntoPartial[Bar].asOption
      )
      // expected output:
      // Bar(a = "value", b = None)
      // Some(value = Bar(a = "value", b = None))
    }

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
      .enableOptionDefaultsToNone
      .buildTransformer
    ```

The `None` value is used as a fallback, meaning:

  - it has to be enabled with a flag
  - it will not be used if you provided value manually with one of the `.with*` methods - then the value provision
    always succeeds
  - it will not be used if a source field (`val`) or a method (enabled with one of the flags above) with a matching name
    could be found - if a source value type can be converted into a target argument/setter type then the value provision
    succeeds, but if Chimney fails to convert the value then the whole derivation fails rather than falls back to
    the `None` value
  - it will not be used if a default value is present and [the support for default values has been enabled](#allowing-fallback-to-the-constructors-default-values)
    (the fallback to `None` has a lower priority than the fallback to a default value) 

!!! example

    Behavior when both [`.enableDefaultValues`](#allowing-fallback-to-the-constructors-default-values) and `.enableOptionDefaultsToNone` are used:

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String)
    case class Bar(a: String, b: Option[String] = Some("a"))

    pprint.pprintln(
      Foo("value").into[Bar].enableDefaultValues.enableOptionDefaultsToNone.transform
    )
    pprint.pprintln(
      Foo("value").intoPartial[Bar].enableDefaultValues.enableOptionDefaultsToNone.transform.asOption
    )
    // expected ouput:
    // Bar(a = "value", b = Some(value = "a"))
    // Some(value = Bar(a = "value", b = Some(value = "a")))

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableOptionDefaultsToNone.enableDefaultValues

      pprint.pprintln(
        Foo("value").transformInto[Bar]
      )
      pprint.pprintln(
        Foo("value").transformIntoPartial[Bar].asOption
      )
      // expected ouput:
      // Bar(a = "value", b = Some(value = "a"))
      // Some(value = Bar(a = "value", b = Some(value = "a")))
    }
    ```
    
    The original default value has a higher priority than `None`.

If the flag was enabled in the implicit config it can be disabled with `.disableOptionDefaultsToNone`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String)
    case class Bar(a: String, b: Option[String] = Some("a"))

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableOptionDefaultsToNone

    Foo("value").into[Bar].disableOptionDefaultsToNone.transform
    // expected error:
    // Chimney can't derive transformation from Foo to Bar
    //
    // Bar
    //   b: scala.Option[java.lang.String] - no accessor named b in source type Foo
    //
    // There are default values for b, the constructor argument/setter in Bar. Consider using .enableDefaultValues or .enableDefaultValueForType.
    //
    // There are default optional values available for b, the constructor argument/setter in Bar. Consider using .enableOptionDefaultsToNone.
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

### Wiring the constructor's parameter to its source field

In some cases, there is no source field available of the same name as the constructor's argument. However, another field
could be used in this role. Other times the source field of the matching name exists, but we want to explicitly override
it with another field. Since the usual cause of such cases is a _rename_, we can handle it using `.withFieldRenamed`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: Int)
    case class Bar(a: String, c: Int)

    pprint.pprintln(
      Foo("value", 1248).into[Bar].withFieldRenamed(_.b, _.c).transform
    )
    pprint.pprintln(
      Foo("value", 1248).intoPartial[Bar].withFieldRenamed(_.b, _.c).transform.asEither
    )
    // expected output:
    // Bar(a = "value", c = 1248)
    // Right(value = Bar(a = "value", c = 1248))

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer with overrides we can create implicits using
    val transformer: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
      .withFieldRenamed(_.b, _.c)
      .buildTransformer
    ```

!!! tip

    The intuition is that we are pointing at a field in a source `case class` then a field in target `case class`, and
    Chimney will use the value from the former to provide it to the latter.
    
    However, Chimney is **not** limited to `case class`es and we can provide a value for **every** constructor's
    argument as long as it has a matching `val`, that we can use in `_.targetName` hint.

The requirements to use a rename are as follows:

  - you have to pass `_.fieldName` directly, it cannot be done with a reference to the function
  - you have to have a `val`/nullary method/Bean getter with a name matching constructor's argument (or Bean setter if
    setters are enabled) to point which argument you are targeting
  - the field rename can be _nested_, you can pass `_.foo.bar.baz` there, additionally you can use:
     - `.matching[Subtype]` to select just one subtype of ADT e.g `_.adt.matching[Subtype].subtypeField` (not
       recommended for matching on `Option` or `Either`, as the subtype might be lengthy and require a subsequent
       `.value`. Use dedicated matchers described below).
     - `.matchingSome` to select values inside `Option` e.g. `_.option.matchingSome.field`
     - `.matchingLeft` and `.matchingRight` to select values inside `Either` e.g. `_.either.matchingLeft.field` or
       `_.either.matchingRight.field`
     - `.everyItem` to select items inside collection or array e.g. `_.list.everyItem.field`, `_.array.everyItem.field`
     - `.everyMapKey` and `.everyMapValue` to select keys/values inside maps e.g. `_.map.everyMapKey.field`,
       `_.map.everyMapValue.field`
     - selectors for collections/`Option`s/`Either`s must be possible to implement, so e.g. you cannot rename from
       `_.everyItem.fieldName` into `_.fieldNameOutsideCollection` 
 
The first 2 conditions are always met when working with: `case class`es with no `private val`s in constructor, classes
with all arguments declared as public `val`s, and Java Beans where each setter has a corresponding getter defined.

!!! example

    Field renaming with Java Beans 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    class Foo() {
      def getA: String = "value"
      def getB(): Int = 777
    }
    class Bar() {
      private var a = ""
      def getA(): String = a
      def setA(aa: String): Unit = a = aa
      private var c = 0
      def getC: Int = c
      def setC(cc: Int): Unit = c = cc
    }

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

    (new Foo())
      .into[Bar]
      .withFieldRenamed(_.getB(), _.getC)
      .transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA)
    // bar.setC(foo.getB())
    // bar
    (new Foo())
      .intoPartial[Bar]
      .withFieldRenamed(_.getB(), _.getC)
      .transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA)
    // bar.setC(foo.getB())
    // partial.Result.fromValue(bar)
    ```

We are also able to rename fields in nested structure:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: Int)
    case class Bar(a: String, c: Int)

    case class NestedFoo(foo: Foo)
    case class NestedBar(bar: Bar)

    pprint.pprintln(
      NestedFoo(Foo("value", 1248))
        .into[NestedBar]
        .withFieldRenamed(_.foo, _.bar)
        .withFieldRenamed(_.foo.b, _.bar.c)
        .transform
    )
    // expected output:
    // NestedBar(bar = Bar(a = "value", c = 1248))
    pprint.pprintln(
      NestedFoo(Foo("value", 1248))
        .intoPartial[NestedBar]
        .withFieldRenamed(_.foo, _.bar)
        .withFieldRenamed(_.foo.b, _.bar.c)
        .transform
        .asEither
    )
    // expected output:
    // Right(value = NestedBar(bar = Bar(a = "value", c = 1248)))
    ```

including collections:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: Int)
    case class Bar(a: String, c: Int)

    case class NestedFoo(foo: Foo)
    case class NestedBar(bar: Bar)

    pprint.pprintln(
      List(NestedFoo(Foo("value", 1248)))
        .into[Vector[NestedBar]]
        .withFieldRenamed(_.everyItem.foo, _.everyItem.bar)
        .withFieldRenamed(_.everyItem.foo.b, _.everyItem.bar.c)
        .transform
    )
    // expected output:
    // Vector(NestedBar(bar = Bar(a = "value", c = 1248)))
    pprint.pprintln(
      List(NestedFoo(Foo("value", 1248)))
        .intoPartial[Vector[NestedBar]]
        .withFieldRenamed(_.everyItem.foo, _.everyItem.bar)
        .withFieldRenamed(_.everyItem.foo.b, _.everyItem.bar.c)
        .transform
        .asEither
    )
    // expected output:
    // Right(value = Vector(NestedBar(bar = Bar(a = "value", c = 1248))))
    ```

### Wiring the constructor's parameter to a provided value

Another way of handling the missing source field - or overriding an existing one - is providing the value for 
the constructor's argument/setter yourself. The successful value can be provided using `.withFieldConst`:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)

    // Providing missing value...:
    pprint.pprintln(
      Foo("value", 10).into[Bar].withFieldConst(_.c, 1000L).transform
    )
    pprint.pprintln(
      Foo("value", 10).intoPartial[Bar].withFieldConst(_.c, 1000L).transform.asEither
    )
    // expected output:
    // Bar(a = "value", b = 10, c = 1000L)
    // Right(value = Bar(a = "value", b = 10, c = 1000L))
    
    // ...and overriding existing value:
    pprint.pprintln(
      Foo("value", 10).into[Bar].withFieldConst(_.c, 1000L).withFieldConst(_.b, 20).transform
    )
    pprint.pprintln(
      Foo("value", 10).intoPartial[Bar].withFieldConst(_.c, 1000L).withFieldConst(_.b, 20).transform.asEither
    )
    // expected output:
    // Bar(a = "value", b = 20, c = 1000L)
    // Right(value = Bar(a = "value", b = 20, c = 1000L))

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
      .withFieldConst(_.c, 1000L)
      .withFieldConst(_.b, 20)
      .buildTransformer
    ```

`.withFieldConst` can be used to provide/override only _successful_ values. What if we want to provide a failure, e.g.:

  - a `String` with an error message
  - an `Exception`
  - or a notion of the empty value?

These cases can be handled only with `PartialTransformer` using `.withFieldConstPartial`:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)

    // Successful partial.Result constant:
    pprint.pprintln(
      Foo("value", 10)
        .intoPartial[Bar]
        .withFieldConstPartial(_.c, partial.Result.fromValue(100L))
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = Bar(a = "value", b = 10, c = 100L))    

    // A few different partial.Result failures constants:
    pprint.pprintln(
      Foo("value", 10)
        .intoPartial[Bar]
        .withFieldConstPartial(_.c, partial.Result.fromEmpty)
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    pprint.pprintln(
      Foo("value", 10)
        .intoPartial[Bar]
        .withFieldConstPartial(_.c, partial.Result.fromErrorThrowable(new NullPointerException))
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    pprint.pprintln(
      Foo("value", 10)
        .intoPartial[Bar]
        .withFieldConstPartial(_.c, partial.Result.fromErrorString("bad value"))
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:    
    // Left(value = List(("<const for _.c>", EmptyValue)))
    // Left(
    //   value = List(("<const for _.c>", ThrowableMessage(throwable = java.lang.NullPointerException)))
    // )
    // Left(value = List(("<const for _.c>", StringMessage(message = "bad value"))))
    ``` 

As you can see, the transformed value will automatically preserve the field name for which a failure happened.

!!! tip

    The intuition is that we are pointing at a field in a `case class` and provide a value for it.
    
    However, Chimney is **not** limited to `case class`es and we can provide a value for **every** constructor's
    argument as long as it has a matching `val`, that we can use in `_.targetName` hint.

The requirements to use a value provision are as follows:

  - you have to pass `_.fieldName` directly, it cannot be done with a reference to the function
  - you have to have a `val`/nullary method/Bean getter with a name matching constructor's argument (or Bean setter if
    setters are enabled)
  - the path can be _nested_, you can pass `_.foo.bar.baz` there, and additionally you can use:
     - `.matching[Subtype]` to select just one subtype of ADT e.g `_.adt.matching[Subtype].subtypeField` (not
       recommended for matching on `Option` or `Either`, as the subtype might be lengthy and require a subsequent
       `.value`. Use dedicated matchers described below).
     - `.matchingSome` to select values inside `Option` e.g. `_.option.matchingSome.field`
     - `.matchingLeft` and `.matchingRight` to select values inside `Either` e.g. `_.either.matchingLeft.field` or
       `_.either.matchingRight.field`
     - `.everyItem` to select items inside collection or array e.g. `_.list.everyItem.field`, `_.array.everyItem.field`
     - `.everyMapKey` and `.everyMapValue` to select keys/values inside maps e.g. `_.map.everyMapKey.field`,
       `_.map.everyMapValue.field`

The second condition is always met when working with `case class`es with no `private val`s in constructor, and classes
with all arguments declared as public `val`s, and Java Beans where each setter has a corresponding getter defined.

!!! example

    Value provision with Java Beans 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    class Foo() {
      def getA: String = "value"
      def getB(): Int = 777
    }
    class Bar() {
      private var a = ""
      def getA(): String = a
      def setA(aa: String): Unit = a = aa
      private var c = 0
      def getC: Int = c
      def setC(cc: Int): Unit = c = cc
    }

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

    (new Foo())
      .into[Bar]
      .withFieldConst(_.getC, 100)
      .transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA)
    // bar.setC(100L)
    // bar
    (new Foo())
      .intoPartial[Bar]
      .withFieldConstPartial(_.getC, partial.Result.fromEmpty)
      .transform
    // val foo = new Foo()
    // partial.Result.fromEmpty[Long].map { c =>
    //   val bar = new Bar()
    //   bar.setA(foo.getA)
    //   bar.setC(c)
    //   bar
    // }
    ```

We are also able to provide values in nested structure:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)

    case class NestedFoo(foo: Foo)
    case class NestedBar(bar: Bar)

    pprint.pprintln(
      NestedFoo(Foo("value", 1248))
        .into[NestedBar]
        .withFieldRenamed(_.foo, _.bar)
        .withFieldConst(_.bar.c, 1000L)
        .transform
    )
    // expected output:
    // NestedBar(bar = Bar(a = "value", b = 1248, c = 1000L))
    
    pprint.pprintln(
      NestedFoo(Foo("value", 1248))
        .intoPartial[NestedBar]
        .withFieldRenamed(_.foo, _.bar)
        .withFieldConst(_.bar.c, 1000L)
        .transform
        .asEither
    )
    // expected output:
    // Right(value = NestedBar(bar = Bar(a = "value", b = 1248, c = 1000L)))
    ```

### Wiring the constructor's parameter to the computed value

Yet another way of handling the missing source field - or overriding an existing one - is computing the value for 
the constructor's argument/setter out from a whole transformed value. The always-succeeding transformation can be provided
using `.withFieldComputed`:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)

    // Providing missing value...:
    pprint.pprintln(
      Foo("value", 10).into[Bar].withFieldComputed(_.c, foo => foo.b.toLong * 2).transform
    )
    pprint.pprintln(
      Foo("value", 10).intoPartial[Bar].withFieldComputed(_.c, foo => foo.b.toLong * 2).transform.asEither
    )
    // expected output:
    // Bar(a = "value", b = 10, c = 20L)
    // Right(value = Bar(a = "value", b = 10, c = 20L))

    // ...and overriding existing value:
    pprint.pprintln(
      Foo("value", 10)
        .into[Bar]
        .withFieldComputed(_.c, foo => foo.b.toLong * 2)
        .withFieldComputed(_.b, foo => foo.b * 4)
        .transform
    )
    pprint.pprintln(
      Foo("value", 10)
        .intoPartial[Bar]
        .withFieldComputed(_.c, foo => foo.b.toLong * 2)
        .withFieldComputed(_.b, foo => foo.b * 4)
        .transform
        .asEither
    )
    // expected output:
    // Bar(a = "value", b = 40, c = 20L)
    // Right(value = Bar(a = "value", b = 40, c = 20L))
    
    // We can also use values extracted from the source:
    pprint.pprintln(
      List(Foo("value", 10))
        .into[Vector[Bar]]
        .withFieldComputedFrom(_.everyItem.b)(_.everyItem.c, _.toLong * 2)
        .withFieldComputedFrom(_.everyItem.b)(_.everyItem.b, _ * 4)
        .transform
    )
    pprint.pprintln(
      List(Foo("value", 10))
        .intoPartial[Vector[Bar]]
        .withFieldComputedFrom(_.everyItem.b)(_.everyItem.c, _.toLong * 2)
        .withFieldComputedFrom(_.everyItem.b)(_.everyItem.b, _ * 4)
        .transform
        .asEither
    )
    // expected output:
    // Vector(Bar(a = "value", b = 40, c = 20L))
    // Right(value = Vector(Bar(a = "value", b = 40, c = 20L)))

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
      .withFieldComputed(_.c, foo => foo.b.toLong * 2)
      .withFieldComputed(_.b, foo => foo.b * 4)
      .buildTransformer
    ```

`.withFieldComputed`/`.withFieldComputedFrom` can be used to compute only _successful_ values. What if we want to
provide a failure, e.g.:

  - a `String` with an error message
  - an `Exception`
  - or a notion of the empty value?

These cases can be handled only with `PartialTransformer` using
`.withFieldComputedPartial`/.`withFieldComputedPartialFrom`:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)

    // Always successful partial.Result:
    pprint.pprintln(
      Foo("value", 10)
        .intoPartial[Bar]
        .withFieldComputedPartial(_.c, foo => partial.Result.fromValue(foo.b.toLong * 2))
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = Bar(a = "value", b = 10, c = 20L))
    
    // Always failing with a partial.Result.fromErrorString:
    pprint.pprintln(
      Foo("value", 10)
        .intoPartial[Bar]
        .withFieldComputedPartial(_.c, foo => partial.Result.fromErrorString("bad value"))
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Left(value = List(("<computed for _.c>", StringMessage(message = "bad value"))))
    
    // Failure depends on the input (whether .toLong throws or not):
    pprint.pprintln(
      Foo("20", 10)
        .intoPartial[Bar]
        .withFieldComputedPartial(_.c, foo => partial.Result.fromCatching(foo.a.toLong))
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    pprint.pprintln(
      Foo("value", 10)
        .intoPartial[Bar]
        .withFieldComputedPartial(_.c, foo => partial.Result.fromCatching(foo.a.toLong))
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = Bar(a = "20", b = 10, c = 20L))
    // Left(
    //   value = List(
    //     (
    //       "<computed for _.c>",
    //       ThrowableMessage(throwable = java.lang.NumberFormatException: For input string: "value")
    //     )
    //   )
    // )
    
    // We can also use values extracted from the source:
    pprint.pprintln(
      List(Foo("20", 10))
        .intoPartial[Vector[Bar]]
        .withFieldComputedPartialFrom(_.everyItem.a)(_.everyItem.c, a => partial.Result.fromCatching(a.toLong))
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    pprint.pprintln(
      List(Foo("value", 10))
        .intoPartial[Vector[Bar]]
        .withFieldComputedPartialFrom(_.everyItem.a)(_.everyItem.c, a => partial.Result.fromCatching(a.toLong))
        .transform
        .asEither
    )
    // expected output:
    // Right(value = Vector(Bar(a = "20", b = 10, c = 20L)))
    // Left(
    //   value = Errors(
    //     errors = NonEmptyErrorsChain(
    //       Error(
    //         message = ThrowableMessage(
    //           throwable = java.lang.NumberFormatException: For input string: "value"
    //         ),
    //         path = Path(elements = List(Computed(targetPath = "_.everyItem.c"), Index(index = 0)))
    //       )
    //     )
    //   )
    // )
    ``` 

As you can see, the transformed value will automatically preserve the field name for which failure happened.

!!! tip

    The intuition is that we are pointing at a field in a `case class` and computing a value for it.
    
    However, Chimney is **not** limited to `case class`es and we can compute a value for **every** constructor's
    argument as long as it has a matching `val`, that we can use in `_.targetName` hint.

The requirements to use a value computation are as follows:

  - you have to pass `_.fieldName` directly, it cannot be done with a reference to the function
  - you have to have a `val`/nullary method/Bean getter with a name matching constructor's argument (or Bean setter if
    setters are enabled)
  - the path can be _nested_, you can pass `_.foo.bar.baz` there, and additionally you can use:
     - `.matching[Subtype]` to select just one subtype of ADT e.g `_.adt.matching[Subtype].subtypeField` (not
       recommended for matching on `Option` or `Either`, as the subtype might be lengthy and require a subsequent
       `.value`. Use dedicated matchers described below).
     - `.matchingSome` to select values inside `Option` e.g. `_.option.matchingSome.field`
     - `.matchingLeft` and `.matchingRight` to select values inside `Either` e.g. `_.either.matchingLeft.field` or
       `_.either.matchingRight.field`
     - `.everyItem` to select items inside collection or array e.g. `_.list.everyItem.field`, `_.array.everyItem.field`
     - `.everyMapKey` and `.everyMapValue` to select keys/values inside maps e.g. `_.map.everyMapKey.field`,
       `_.map.everyMapValue.field`

The second condition is always met when working with `case class`es with no `private val`s in constructor, and classes
with all arguments declared as public `val`s, and Java Beans where each setter has a corresponding getter defined.

!!! example

    Value computation with Java Beans 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    class Foo() {
      def getA: String = "value"
      def getB(): Int = 777
    }
    class Bar() {
      private var a = ""
      def getA(): String = a
      def setA(aa: String): Unit = a = aa
      private var c = 0L
      def getC: Long = c
      def setC(cc: Long): Unit = c = cc
    }

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

    (new Foo())
      .into[Bar]
      .withFieldComputed(_.getC, foo => foo.getB().toLong)
      .transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA)
    // bar.setC(100L)
    // bar
    (new Foo())
      .intoPartial[Bar]
      .withFieldComputedPartial(_.getC, foo => partial.Result.fromCatching(foo.getA.toLong))
      .transform
    // val foo = new Foo()
    // partial.Result.fromCatched(foo.getA.toLong).map { c =>
    //   val bar = new Bar()
    //   bar.setA(foo.getA())
    //   bar.setC(c)
    //   bar
    // }
    ```

We are also able to compute values in nested structure:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)

    case class NestedFoo(foo: Foo)
    case class NestedBar(bar: Bar)

    pprint.pprintln(
      NestedFoo(Foo("value", 1248))
        .into[NestedBar]
        .withFieldRenamed(_.foo, _.bar)
        .withFieldComputed(_.bar.c, nestedfoo => nestedfoo.foo.b.toLong * 2)
        .transform
    )
    // expected output:
    // NestedBar(bar = Bar(a = "value", b = 1248, c = 2496L))
    
    pprint.pprintln(
      NestedFoo(Foo("value", 1248))
        .intoPartial[NestedBar]
        .withFieldRenamed(_.foo, _.bar)
        .withFieldComputedPartial(_.bar.c, nestedfoo => partial.Result.fromValue(nestedfoo.foo.b.toLong * 2))
        .transform
        .asEither
    )
    // expected output:
    // Right(value = NestedBar(bar = Bar(a = "value", b = 1248, c = 2496L)))
    ```

### Customizing field name matching

Be default names are matched in a Java-Bean-aware way - `fieldName` would be considered a match for another `fieldName`
but also for `isFieldName`, `getFieldName` and `setFieldName`. This allows the macro to read both normal `val`s and
Bean getters and write into constructor arguments and Bean setters. (Whether such getters/setters would we admitted
for matching is controlled by dedicated flags: [`.enableBeanGetters`](#reading-from-bean-getters) and 
[`.enableBeanSetters`](#writing-to-bean-setters)).

The field name matching predicate can be overridden with a flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(Baz: Foo.Baz, A: Int)
    object Foo {
      case class Baz(S: String)
    }

    case class Bar(baz: Bar.Baz, a: Int)
    object Bar {
      case class Baz(s: String)
    }

    // Foo(Foo.Baz("test"), 1024).transformInto[Bar] or
    // Foo(Foo.Baz("test"), 1024).into[Bar].transform results in:
    // Chimney can't derive transformation from Foo to Bar
    //
    // Bar
    //   baz: Bar.Baz - no accessor named baz in source type Foo
    //   a: scala.Int - no accessor named a in source type Foo
    //
    // Consult https://chimney.readthedocs.io for usage examples.

    pprint.pprintln(
      Foo(Foo.Baz("test"), 1024)
        .into[Bar]
        .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
    )
    pprint.pprintln(
      Foo(Foo.Baz("test"), 1024)
        .intoPartial[Bar]
        .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
        .asEither
    )
    // expected output:
    // Bar(baz = Baz(s = "test"), a = 1024)
    // Right(value = Bar(baz = Baz(s = "test"), a = 1024))

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default
        .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)

      pprint.pprintln(
        Foo(Foo.Baz("test"), 1024).transformInto[Bar]
      )
      pprint.pprintln(
        Foo(Foo.Baz("test"), 1024).into[Bar].transform
      )
      // expected output:
      // Bar(baz = Baz(s = "test"), a = 1024)
      // Bar(baz = Baz(s = "test"), a = 1024)
      
      pprint.pprintln(
        Foo(Foo.Baz("test"), 1024).transformIntoPartial[Bar].asEither
      )
      pprint.pprintln(
        Foo(Foo.Baz("test"), 1024).intoPartial[Bar].transform.asEither
      )
      // expected output:
      // Right(value = Bar(baz = Baz(s = "test"), a = 1024))
      // Right(value = Bar(baz = Baz(s = "test"), a = 1024))
    }

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
      .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
      .buildTransformer
    ```

For details about `TransformedNamesComparison` look at [their dedicated section](#defining-custom-name-matching-predicate).

!!! warning

    Using a predicate can result in an ambiguity. For instance, if the source would have `val name` and `def getName`,
    the target would have a constructor's argument `name`, `BeanAware` matching was used together with Bean getters flag,
    it would be ambiguous which value shoud be used as a source value.

    However, when a predicate is used for matching the source's values with the target's constructor parameters/setters it
    is also used for matching manual overrides with parameters/setters.

    For instance `BeanAware` setter is used by defauly to allow using the getter `getName` to provide override for 
    the `setName` setter (e.g. `.withFieldConst(_.getName(), value)` would provide value for `setName` setter).

    However, if you named your constructor parameters e.g. `names` and `setNames` then it it would create an ambiguity
    in override matching.

    The former ambiguity can be resolved e.g. by providing value using one of `.withField*` overrides. The later requires
    opting out of any smart matching and only relying on `StrictEquality` and manual overrides.

If the flag was enabled in the implicit config it can be disabled with `.disableCustomFieldNameComparison`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class Foo(Baz: Foo.Baz, A: Int)
    object Foo {
      case class Baz(S: String)
    }

    case class Bar(baz: Bar.Baz, a: Int)
    object Bar {
      case class Baz(s: String)
    }

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default
      .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)

    Foo(Foo.Baz("test"), 1024).into[Bar].disableCustomFieldNameComparison.transform
    // expected error:
    // Chimney can't derive transformation from Foo to Bar
    //
    // Bar
    //   baz: Bar.Baz - no accessor named baz in source type Foo
    //   a: scala.Int - no accessor named a in source type Foo
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

## From/into a `Tuple`

Conversion from/to a tuple of any size is almost identical to conversion between other classes. The only difference
is that when either the source or target type is a tuple, automatic matching between the source field and the target
constructor's argument is made by position instead of name:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: Int, c: Long)

    pprint.pprintln(
      Foo("value", 42, 1024L).transformInto[(String, Int, Long)]
    )
    // expected output:
    // ("value", 42, 1024L)
    pprint.pprintln(
      ("value", 42, 1024L).transformInto[Foo]
    )
    // expected output:
    // Foo(a = "value", b = 42, c = 1024L)
    pprint.pprintln(
      Foo("value", 42, 1024L).transformIntoPartial[(String, Int, Long)].asEither
    )
    // expected output:
    // Right(value = ("value", 42, 1024L))
    pprint.pprintln(
      ("value", 42, 1024L).transformIntoPartial[Foo].asEither
    )
    // expected output:
    // Right(value = Foo(a = "value", b = 42, c = 1024L))

    import io.scalaland.chimney.{Transformer, PartialTransformer}

    // If we want to reuse Transformer, we can create implicits using:
    val totalTransformer: Transformer[Foo, (String, Int, Long)] = Transformer.derive[Foo, (String, Int, Long)]
    val partialTransformer: PartialTransformer[Foo, (String, Int, Long)] = PartialTransformer.derive[Foo, (String, Int, Long)]
    // or (if you want to pass overrides):
    val totalTransformer2: Transformer[Foo, (String, Int, Long)] = Transformer.define[Foo, (String, Int, Long)]
      .buildTransformer
    val partialTransformer2: PartialTransformer[Foo, (String, Int, Long)] = PartialTransformer.define[Foo, (String, Int, Long)]
      .buildTransformer
    ```

!!! tip

    You can use all the flags, renames, value provisions, and computations that are available to case classes,
    Java Beans and so on.

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: Int, c: Long)

    pprint.pprintln(
      Foo("value", 42, 1024L).into[(String, Int, Long, String, Double, Long, Option[Double])]
        .withFieldRenamed(_.a, _._4) // _4
        .withFieldConst(_._5, 3.14) // _5
        .withFieldComputed(_._6, foo => foo.c + 2) // _6
        .withTargetFlag(_._7).enableOptionDefaultsToNone // _7
        .transform
    )
    // expected output:
    // ("value", 42, 1024L, "value", 3.14, 1026L, None)
    ```

!!! tip

    If you are not sure whether the derivation treats your case as tuple conversion, [try enabling macro logging](troubleshooting.md#debugging-macros).

## From/into an `AnyVal`

`AnyVal`s can be used both as data sources for derivation as well as the targets of the transformation.

If `AnyVal` is the source, Chimney would attempt to unwrap it, and if it's the target wrap it - we treat `AnyVal`s
as transparent, similarly to virtually every other Scala library.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: Int) extends AnyVal
    case class Bar(b: Int) extends AnyVal

    pprint.pprintln(
      Foo(10).into[Bar].transform
    )
    pprint.pprintln(
      Foo(10).transformInto[Bar]
    )
    // expected output:
    // Bar(b = 10)
    // Bar(b = 10)
    
    pprint.pprintln(
      Foo(10).transformIntoPartial[Bar].asEither
    )
    pprint.pprintln(
      Foo(10).intoPartial[Bar].transform.asEither
    )
    // expected output:
    // Right(value = Bar(b = 10))
    // Right(value = Bar(b = 10))

    import io.scalaland.chimney.{Transformer, PartialTransformer}

    // If we want to reuse Transformer, we can create implicits using:
    val totalTransformer: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]
    val partialTransformer: PartialTransformer[Foo, Bar] = PartialTransformer.derive[Foo, Bar]
    // or (if you want to pass overrides):
    val totalTransformer2: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
      .buildTransformer
    val partialTransformer2: PartialTransformer[Foo, Bar] = PartialTransformer.define[Foo, Bar]
      .buildTransformer
    ```

!!! tip

    This behavior is non-configurable in Chimney, similar to how it is non-configurable in every other library. If you
    decided to use a derivation then libraries will wrap and upwrap `AnyVal`s for you automatically.
    
    If you don't want this behavior you can prevent it (in every library, not only Chimney) by making the `val`
    `private` - to prevent unwrapping - and/or making the constructor `private` - to prevent wrapping. This way you'd
    have to provide support for your type for each library by yourself.
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class Foo(private val a: Int) extends AnyVal // cannot be automatically unwrapped
    case class Bar private (b: String) extends AnyVal // cannot be automatically wrapped

    Foo(10).transformInto[Bar]
    // expected error:
    // Chimney can't derive transformation from Foo to Bar
    //
    // Bar
    //   derivation from foo: Foo to Bar is not supported in Chimney!
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

!!! tip

    When `AnyVal` special handling cannot be used (e.g. because value/constructor is private), then Chimney falls back
    to treat them as a normal class.

!!! tip

    You can use all the flags, renames, value provisions, and computations that are available to case classes,
    Java Beans and so on.

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: Int) extends AnyVal
    case class Bar(b: Int) extends AnyVal

    pprint.pprintln(
      Foo(10).into[Bar]
        .withFieldComputed(_.b, foo => foo.a + 5)
        .transform
    )
    // expected output:
    // 15
    ```

### From/into a wrapper type

Automatic unwrapping/wrapping is limited only to classes with a single, public `val` constructor parameter, and only
when the whole type `extends AnyVal`. What if we have a type which wraps a single value but does not `extends AnyVal`?

Such cases are often when we use ScalaPB, so it would be useful to automatically handle such cases. It's possible with
a flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    
    case class UserName(value: String)
    
    pprint.pprintln(
      "user name".into[UserName].enableNonAnyValWrappers.transform
    )
    pprint.pprintln(
      "user name".intoPartial[UserName].enableNonAnyValWrappers.transform.asEither
    )
    // expected output:
    // UserName(value = "user name")
    // Right(value = UserName(value = "user name"))
    
    pprint.pprintln(
      UserName("user name").into[String].enableNonAnyValWrappers.transform
    )
    pprint.pprintln(
      UserName("user name").intoPartial[String].enableNonAnyValWrappers.transform.asEither
    )
    // expected output:
    // "user name"
    // Right(value = "user name")
    
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers
      
      pprint.pprintln(
        "user name".transformInto[UserName]
      )
      pprint.pprintln(
        "user name".into[UserName].transform
      )
      // expected output:
      // UserName(value = "user name")
      // UserName(value = "user name")
      // Right(value = UserName(value = "user name"))
      
      pprint.pprintln(
        "user name".transformIntoPartial[UserName].asEither
      )
      pprint.pprintln(
        "user name".intoPartial[UserName].transform.asEither
      )
      // expected output:
      // Right(value = UserName(value = "user name"))
      // Right(value = UserName(value = "user name"))
    }

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[String, UserName] = Transformer.define[String, UserName]
      .enableNonAnyValWrappers
      .buildTransformer
    ```

!!! tip

    If you are not sure whether the derivation treats your case as wrapper conversion, [try enabling macro logging](troubleshooting.md#debugging-macros).

If the flag was enabled in the implicit config it can be disabled with `.disbleNonAnyValWrappers`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._
    
    implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers
    
    case class UserName(value: String)
    
    "user name".into[UserName].disableNonAnyValWrappers.transform
    // expected error:
    // Chimney can't derive transformation from java.lang.String to UserName
    // 
    // UserName
    //   value: java.lang.String - no accessor named value in source type java.lang.String
    // 
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

## Between `sealed`/`enum`s

When both the source type and the target type of the transformation are `sealed` (`trait`, `abstract class`), Chimney
will convert the source type's subtypes into the target type's subtypes. To make it work out of the box, every source
type's subtype needs to have a corresponding subtype with a matching name in the target type:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    sealed trait Foo
    object Foo {
      case class Baz(a: String, b: Int) extends Foo
      case object Buzz extends Foo
    }
    sealed trait Bar
    object Bar {
      case class Baz(b: Int) extends Bar
      case object Fizz extends Bar
      case object Buzz extends Bar
    }

    pprint.pprintln(
      (Foo.Baz("value", 10): Foo).transformInto[Bar]
    )
    pprint.pprintln(
      (Foo.Baz("value", 10): Foo).into[Bar].transform
    )
    // expected output:
    // Baz(b = 10)
    // Baz(b = 10)
    
    pprint.pprintln(
      (Foo.Buzz: Foo).transformInto[Bar]
    )
    pprint.pprintln(
      (Foo.Buzz: Foo).into[Bar].transform
    )
    // expected output:
    // Buzz
    // Buzz
    
    pprint.pprintln(
      (Foo.Baz("value", 10): Foo).transformIntoPartial[Bar].asEither
    )
    pprint.pprintln(
      (Foo.Baz("value", 10): Foo).intoPartial[Bar].transform.asEither
    )
    // expected output:
    // Right(value = Baz(b = 10))
    // Right(value = Baz(b = 10))

    pprint.pprintln(
      (Foo.Buzz: Foo).transformIntoPartial[Bar].asEither
    )
    pprint.pprintln(
      (Foo.Buzz: Foo).intoPartial[Bar].transform.asEither
    )
    // expected output:
    // Right(value = Buzz)
    // Right(value = Buzz)

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer with overrides we can create implicits using
    val transformer: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
      .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
      .buildTransformer
    ```

!!! tip

    You can remember that each `sealed`/`enum` would have to implement an exhaustive pattern matching to handle a whole
    input, and subtypes are matched by their names. So you can have more subtypes in the target type than there are in
    the source type. What you cannot have is a missing match.

It works also with Scala 3's `enum`:

!!! example

    `sealed trait` into `enum`

    ```scala
    // file: snippet.scala - part of sealed trait into Scala 3 enum example
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl.*

    sealed trait Foo
    object Foo:
      case class Baz(a: String, b: Int) extends Foo
      case object Buzz extends Foo
    enum Bar:
      case Baz(b: Int)
      case Fizz
      case Buzz

    @main def example: Unit = {
    pprint.pprintln(
      (Foo.Baz("value", 10): Foo).transformInto[Bar]
    )
    pprint.pprintln(
      (Foo.Buzz: Foo).transformInto[Bar]
    )
    // expected output:
    // Baz(b = 10)
    // Buzz
    }
    ```
    
!!! example

    `enum` into `sealed trait` 

    ```scala
    // file: snippet.scala - part of Scala 3 enum into sealed trait example
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl.*

    enum Foo:
      case Baz(a: String, b: Int)
      case Buzz
    sealed trait Bar
    object Bar:
      case class Baz(b: Int) extends Bar
      case object Fizz extends Bar
      case object Buzz extends Bar

    @main def example: Unit = {
    pprint.pprintln(
      (Foo.Baz("value", 10): Foo).transformInto[Bar]
    )
    pprint.pprintln(
      (Foo.Buzz: Foo).transformInto[Bar]
    )
    // expected output:
    // Baz(b = 10)
    // Buzz
    }
    ```
    
!!! example

    `enum` into `enum` 

    ```scala
    // file: snippet.scala - part of Scala 3 enum into Scala 3 enum example
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl.*

    enum Foo:
      case Baz(a: String, b: Int)
      case Buzz
    enum Bar:
      case Baz(b: Int)
      case Fizz
      case Buzz

    @main def example: Unit = {
    pprint.pprintln(
      (Foo.Baz("value", 10): Foo).transformInto[Bar]
    )
    pprint.pprintln(
      (Foo.Buzz: Foo).transformInto[Bar]
    )
    // expected output:
    // Baz(b = 10)
    // Buzz
    }
    ```

### Non-flat ADTs

To enable seamless work with [Protocol Buffers](cookbook.md#protocol-buffers-integration), there is also a special
handling for non-flat ADTs, where each subtype of a `sealed`/`enum` is a single-value wrapper around a `case class`.
In such cases, Chimney is able to automatically wrap/unwrap these inner values as if they were `AnyVal`s
(even though they are not!):

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    object protobuf {
      sealed trait Foo
      object Foo {
        case class A(a: String, b: Int)
        case class B()
      }
      case class A(value: Foo.A) extends Foo
      case class B(value: Foo.B) extends Foo
    }

    object domain {
      sealed trait Bar
      object Bar {
        case class A(a: String, b: Int) extends Bar
        case object B extends Bar
      }
    }

    // flattening
    pprint.pprintln(
      (protobuf.A(protobuf.Foo.A("value", 42)): protobuf.Foo).transformInto[domain.Bar]
    )
    pprint.pprintln(
      (protobuf.B(protobuf.Foo.B()): protobuf.Foo).transformInto[domain.Bar]
    )
    // expected output:
    // A(a = "value", b = 42)
    // B
    
    // unflattening
    pprint.pprintln(
      (domain.Bar.A("value", 42): domain.Bar).transformInto[protobuf.Foo]
    )
    pprint.pprintln(
      (domain.Bar.B: domain.Bar).transformInto[protobuf.Foo]
    )
    // expected output:
    // A(value = A(a = "value", b = 42))
    // B(value = B())

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[protobuf.Foo, domain.Bar] = Transformer.derive[protobuf.Foo, domain.Bar]
    ```

### Java's `enum`s

Java's `enum` can also be converted this way to/from `sealed`/Scala 3's `enum`/another Java's `enum`:

!!! example

    Java's `enum` to/from `sealed`

    ```java
    // file: ColorJ.java - part of Java enum and Scala 2 example
    enum ColorJ {
      Red, Green, Blue;
    }
    ```

    ```scala
    // file: example.sc - part of Java enum and Scala 2 example
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    sealed trait ColorS
    object ColorS {
      case object Red extends ColorS
      case object Green extends ColorS
      case object Blue extends ColorS
    }

    pprint.pprintln(
      ColorJ.Red.transformInto[ColorS]
    )
    pprint.pprintln(
      ColorJ.Green.transformInto[ColorS]
    )
    pprint.pprintln(
      ColorJ.Blue.transformInto[ColorS]
    )
    // expected output:
    // Red
    // Green
    // Blue

    pprint.pprintln(
      (ColorS.Red: ColorS).transformInto[ColorS]
    )
    pprint.pprintln(
      (ColorS.Green: ColorS).transformInto[ColorS]
    )
    pprint.pprintln(
      (ColorS.Blue: ColorS).transformInto[ColorS]
    )
    // expected output:
    // Red
    // Green
    // Blue

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[ColorJ, ColorS] = Transformer.derive[ColorJ, ColorS]
    ```

!!! example

    Java's `enum` to/from Scala's `enum`

    ```java
    // file: ColorJ.java - part of Java enum and Scala 3 example
    enum ColorJ {
      Red, Green, Blue;
    }
    ```

    ```scala
    // file: example.scala - part of Java enum and Scala 3 example
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    enum ColorE:
      case Red, Green, Blue

    @main def example: Unit = {
    pprint.pprintln(
      ColorJ.Red.transformInto[ColorE]
    )
    pprint.pprintln(
      ColorJ.Green.transformInto[ColorE]
    )
    pprint.pprintln(
      ColorJ.Blue.transformInto[ColorE]
    )
    // expected output:
    // Red
    // Green
    // Blue
    
    pprint.pprintln(
      (ColorE.Red: ColorE).transformInto[ColorJ]
    )
    pprint.pprintln(
      (ColorE.Green: ColorE).transformInto[ColorJ]
    )
    pprint.pprintln(
      (ColorE.Blue: ColorE).transformInto[ColorJ]
    )
    // expected output:
    // Red
    // Green
    // Blue
    }
    ```

### Handling a specific `sealed` subtype by a specific target subtype

Sometimes a corresponding subtype of the target type has an unrelated name, that cannot be matched by simple comparison.
Or we might want to redirect two subtypes into the same target subtype. For that we have `.withSealedSubtypeRenamed`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    
    sealed trait Source
    object Source {
      case object Foo extends Source
      case class Baz(a: Int) extends Source
    }
    
    sealed trait Target
    object Target {
      case object Foo extends Target
      case class Bar(a: Int) extends Target
    }
    
    pprint.pprintln(
      (Source.Baz(10): Source).into[Target].withSealedSubtypeRenamed[Source.Baz, Target.Bar].transform
    )
    // expected output:
    // Bar(a = 10)

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Source, Target] = Transformer.define[Source, Target]
      .withSealedSubtypeRenamed[Source.Baz, Target.Bar]
      .buildTransformer
    ```

!!! notice

    If one needs to handle this case in but nested inside a `case class`/`Option`/`Either`/collection, one can use
    `.withFieldRenamed` with proper selectors:

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    
    sealed trait Source
    object Source {
      case object Foo extends Source
      case class Baz(a: Int) extends Source
    }
    
    sealed trait Target
    object Target {
      case object Foo extends Target
      case class Bar(a: Int) extends Target
    }
    
    pprint.pprintln(
      List(Source.Baz(10): Source).into[List[Target]]
        .withFieldRenamed(_.everyItem.matching[Source.Baz], _.everyItem.matching[Target.Bar])
        .transform
    )
    // expected output:
    // List(Bar(a = 10))
    ```

!!! notice

    While `sealed` hierarchies, Scala 3 `enum`s and Java `enum`s fall into the same category of Algebraic Data Types,
    manu users might consider them different things and e.g. not look for methods starting with `.withSealedSubtype`
    when dealing with `enum`s. For that reason we provide an aliases to this methods - `.withEnumCaseRenamed`:

    ```scala
    // file: snippet.scala - part of withEnumCaseRenamed example
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    
    enum Source {
      case Foo
      case Baz(a: Int)
    }
    
    enum Target {
      case Foo
      case Bar(a: Int)
    }
    
    @main def example: Unit = {
    pprint.pprintln(
      (Source.Baz(10): Source).into[Target].withEnumCaseRenamed[Source.Baz, Target.Bar].transform
    )
    // expected output:
    // Bar(a = 10)
    }
    ```
    
    These methods are only aliases and there is no difference in behavior between `.withSealedSubtypeRenamed` and
    `.withEnumCaseRenamed` - the difference in names exist only for the sake of readability and discoverability.

!!! warning

    Due to limitations of Scala 2, when you want to use `.withSealedSubtypeRenamed` with Java's `enum`s, the enum
    instance's exact type will always be upcasted/lost, turning the handler into "catch-all".
    
    The explanation and the solution to that is described in
    [`.withSealedSubtypeHandled`](#handling-a-specific-sealed-subtype-with-a-computed-value) documentation.

### Handling a specific `sealed` subtype with a computed value

Sometimes we are missing a corresponding subtype of the target type. Or we might want to override it with our
computation. This can be done using `.withSealedSubtypeHandled`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    sealed trait Foo
    object Foo {
      case class Baz(a: String) extends Foo
      case object Buzz extends Foo
    }
    sealed trait Bar
    object Bar {
      case class Baz(a: String) extends Bar
      case object Fizz extends Bar
      case object Buzz extends Bar
    }

    pprint.pprintln(
      (Bar.Baz("value"): Bar)
        .into[Foo]
        .withSealedSubtypeHandled[Bar.Fizz.type] { fizz =>
          Foo.Baz(fizz.toString)
        }
        .transform
    )
    // expected output:
    // Baz(a = "value")
    
    pprint.pprintln(
      (Bar.Fizz: Bar)
        .into[Foo]
        .withSealedSubtypeHandled[Bar.Fizz.type] { fizz =>
          Foo.Baz(fizz.toString)
        }
        .transform
    )
    // expected output:
    // Baz(a = "Fizz")
    
    pprint.pprintln(
      (Bar.Buzz: Bar)
        .into[Foo]
        .withSealedSubtypeHandled[Bar.Fizz.type] { fizz =>
          Foo.Baz(fizz.toString)
        }
        .transform
    )
    // expected output:
    // Buzz

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Bar, Foo] = Transformer.define[Bar, Foo]
      .withSealedSubtypeHandled[Bar.Fizz.type] { fizz =>
        Foo.Baz(fizz.toString)
      }
      .buildTransformer
    ```

If the computation needs to allow failure, there is `.withSealedSubtypeHandledPartial`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    sealed trait Foo
    object Foo {
      case class Baz(a: String) extends Foo
      case object Buzz extends Foo
    }
    sealed trait Bar
    object Bar {
      case class Baz(a: String) extends Bar
      case object Fizz extends Bar
      case object Buzz extends Bar
    }

    pprint.pprintln(
      (Bar.Baz("value"): Bar)
        .intoPartial[Foo]
        .withSealedSubtypeHandledPartial[Bar.Fizz.type] { fizz =>
          partial.Result.fromEmpty
        }
        .transform
        .asEither
    )
    // expected output:
    // Right(value = Baz(a = "value"))
    
    pprint.pprintln(
      (Bar.Fizz: Bar)
        .intoPartial[Foo]
        .withSealedSubtypeHandledPartial[Bar.Fizz.type] { fizz =>
          partial.Result.fromEmpty
        }
        .transform
        .asEither
    )
    // expected output:
    // Left(
    //   value = Errors(
    //     errors = NonEmptyErrorsChain(Error(message = EmptyValue, path = Path(elements = List())))
    //   )
    // )
    
    pprint.pprintln(
      (Bar.Buzz: Bar)
        .intoPartial[Foo]
        .withSealedSubtypeHandledPartial[Bar.Fizz.type] { fizz =>
          partial.Result.fromEmpty
        }
        .transform
        .asEither
    )
    // expected output:
    // Right(value = Buzz)
    ```

!!! notice

    If one needs to handle this case in but nested inside a `case class`/`Option`/`Either`/collection, one can use
    `.withFieldComputedFrom`/`.withFieldComputedPartialFrom` with proper selectors:

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    sealed trait Foo
    object Foo {
      case class Baz(a: String) extends Foo
      case object Buzz extends Foo
    }
    sealed trait Bar
    object Bar {
      case class Baz(a: String) extends Bar
      case object Fizz extends Bar
      case object Buzz extends Bar
    }

    pprint.pprintln(
      List(Bar.Baz("value"): Bar, Bar.Fizz: Bar, Bar.Buzz: Bar)
        .into[List[Foo]]
        .withFieldComputedFrom(_.everyItem.matching[Bar.Fizz.type])(_.everyItem, fizz => Foo.Baz(fizz.toString))
        .transform
    )
    // expected output:
    //  List(Baz(a = "value"), Baz(a = "Fizz"), Buzz)
    
    pprint.pprintln(
      List(Bar.Baz("value"): Bar, Bar.Fizz: Bar, Bar.Buzz: Bar)
        .intoPartial[List[Foo]]
        .withFieldComputedPartialFrom(_.everyItem.matching[Bar.Fizz.type])(_.everyItem, fizz => partial.Result.fromEmpty)
        .transform
        .asEither
    )
    // expected output:
    // Left(
    //   value = Errors(
    //     errors = NonEmptyErrorsChain(
    //       Error(message = EmptyValue, path = Path(elements = List(Index(index = 1))))
    //     )
    //   )
    // )
    ```

!!! notice

    While `sealed` hierarchies, Scala 3 `enum`s and Java `enum`s fall into the same category of Algebraic Data Types,
    manu users might consider them different things and e.g. not look for methods starting with `.withSealedSubtype`
    when dealing with `enum`s. For that reason we provide an aliases to both of these methods - `.withEnumCaseHandled`
    and `.withEnumCaseHandledPartial`:
    
    ```scala
    // file: snippet.scala - part of withEnumCaseHandled example
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl.*
    import io.scalaland.chimney.partial

    enum Foo {
      case Baz(a: String)
      case Buzz
    }
    enum Bar {
      case Baz(a: String)
      case Fizz
      case Buzz
    }

    @main def example: Unit = {
    pprint.pprintln(
      (Bar.Baz("value"): Bar)
        .into[Foo]
        .withEnumCaseHandled[Bar.Fizz.type] { fizz =>
          Foo.Baz(fizz.toString)
        }
        .transform
    )
    pprint.pprintln(
      (Bar.Fizz: Bar)
        .into[Foo]
        .withEnumCaseHandled[Bar.Fizz.type] { fizz =>
          Foo.Baz(fizz.toString)
        }
        .transform
    )
    pprint.pprintln(
      (Bar.Buzz: Bar)
        .into[Foo]
        .withEnumCaseHandled[Bar.Fizz.type] { fizz =>
          Foo.Baz(fizz.toString)
        }
        .transform
    )
    // expected output:
    // Baz(a = "value")
    // Baz(a = "Fizz")
    // Buzz

    pprint.pprintln(
      (Bar.Baz("value"): Bar)
        .intoPartial[Foo]
        .withEnumCaseHandledPartial[Bar.Fizz.type] { fizz =>
          partial.Result.fromEmpty
        }
        .transform
        .asEither
    )
    pprint.pprintln(
      (Bar.Fizz: Bar)
        .intoPartial[Foo]
        .withEnumCaseHandledPartial[Bar.Fizz.type] { fizz =>
          partial.Result.fromEmpty
        }
        .transform
        .asEither
    )
    pprint.pprintln(
      (Bar.Buzz: Bar)
        .intoPartial[Foo]
        .withEnumCaseHandledPartial[Bar.Fizz.type] { fizz =>
          partial.Result.fromEmpty
        }
        .transform
        .asEither
    )
    // expected output:
    // Right(value = Baz(a = "value"))
    // Left(
    //   value = Errors(
    //     errors = NonEmptyErrorsChain(Error(message = EmptyValue, path = Path(elements = List())))
    //   )
    // )
    // Right(value = Buzz)
    }
    ```
    
    These methods are only aliases and there is no difference in behavior between `.withSealedCaseHandled` and
    `.withEnumCaseHandled` - the difference in names exist only for the sake of readability and discoverability.

!!! notice

     `.withSealedSubtypeHandled[Subtype](...)` might look similar to `.withFieldComputed(_.matching[Subtype], ...)` but
     the difference becomes clear when we provide the types:
     
      * `foo.into[Bar].withSealedSubtypeHandled[Foo.Baz](subtype => ...).transform` matches the subtype on the **source**
        value, while
      * `foo.into[Bar].withFieldComputed(_.matching[Bar.Baz], foo => ...)` - provides an override on the **target**
        type's value
       
     so these 2 pieces of code covers difference use cases.

!!! warning

    Due to limitations of Scala 2, when you want to use `.withSealedSubtypeHandled` or `.withSealedSubtypeHandledPartial` with
    Java's `enum`s, the enum instance's exact type will always be upcasted/lost, turning the handler into "catch-all":

    ```java
    // file: ColorJ.java - part of Java enums in Scala 2 failure example
    enum ColorJ {
      Red, Blue, Green, Black;
    }
    ```

    ```scala
    // file: example.sc - part of Java enums in Scala 2 failure example
    //> using scala {{ scala.2_13 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    sealed trait ColorS
    object ColorS {
      case object Red extends ColorS
      case object Green extends ColorS
      case object Blue extends ColorS
    }

    def blackIsRed(black: ColorJ.Black.type): ColorS = ColorS.Red

    pprint.pprintln(
      ColorJ.Red.into[ColorS].withSealedSubtypeHandled[ColorJ.Black.type](blackIsRed(_)).transform
    )
    pprint.pprintln(
      ColorJ.Green.into[ColorS].withSealedSubtypeHandled[ColorJ.Black.type](blackIsRed(_)).transform
    )
    pprint.pprintln(
      ColorJ.Blue.into[ColorS].withSealedSubtypeHandled[ColorJ.Black.type](blackIsRed(_)).transform
    )
    pprint.pprintln(
      ColorJ.Black.into[ColorS].withSealedSubtypeHandled[ColorJ.Black.type](blackIsRed(_)).transform
    )
    // expected output:
    // Red
    // Red
    // Red
    // Red
    ```
    
    There is nothing we can do about the type, however, we can analyze the code and, if it preserves the exact Java enum
    we can use a sort of a type refinement to remember the selected instance:
    
    ```java
    // file: ColorJ.java - part of Java enums in Scala 2 workaround example
    enum ColorJ {
      Red, Blue, Green, Black;
    }
    ```

    ```scala
    // file: example.sc - part of Java enums in Scala 2 workaround example
    //> using scala {{ scala.2_13 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    sealed trait ColorS
    object ColorS {
      case object Red extends ColorS
      case object Green extends ColorS
      case object Blue extends ColorS
    }

    def blackIsRed(black: ColorJ.Black.type): ColorS = ColorS.Red

    pprint.pprintln(
      ColorJ.Red.into[ColorS].withSealedSubtypeHandled { (black: ColorJ.Black.type) => blackIsRed(black) }.transform
    )
    pprint.pprintln(
      ColorJ.Green.into[ColorS].withSealedSubtypeHandled { (black: ColorJ.Black.type) => blackIsRed(black) }.transform
    )
    pprint.pprintln(
      ColorJ.Blue.into[ColorS].withSealedSubtypeHandled { (black: ColorJ.Black.type) => blackIsRed(black)}.transform
    )
    pprint.pprintln(
      ColorJ.Black.into[ColorS].withSealedSubtypeHandled { (black: ColorJ.Black.type) => blackIsRed(black) }.transform
    )
    // expected output:
    // Red
    // Green
    // Blue
    // Red
    ```
    
    This issue doesn't occur on Scala 3, which infers types correctly:
    
    ```java
    // file: ColorJ.java - part of Java enums in Scala 3 example
    enum ColorJ {
      Red, Blue, Green, Black;
    }
    ```

    ```scala
    // file: example.scala - part of Java enums in Scala 3 example
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl.*

    enum ColorS:
      case Red, Green, Blue

    def blackIsRed(black: ColorJ.Black.type): ColorS = ColorS.Red

    @main def example: Unit = {
    pprint.pprintln(
      (ColorJ.Red: ColorJ).into[ColorS].withSealedSubtypeHandled(blackIsRed).transform
    )
    pprint.pprintln(
      (ColorJ.Green: ColorJ).into[ColorS].withSealedSubtypeHandled(blackIsRed).transform
    )
    pprint.pprintln(
      (ColorJ.Blue: ColorJ).into[ColorS].withSealedSubtypeHandled(blackIsRed).transform
    )
    pprint.pprintln(
      (ColorJ.Black: ColorJ).into[ColorS].withSealedSubtypeHandled(blackIsRed).transform
    )
    // expected output:
    // Red
    // Green
    // Blue
    // Red
    }
    ```

If one needs to handle this case in but nested inside a `case class`/`Option`/`Either`/collection, one can use
`.withFieldRenamed` with a proper selectors:

### Customizing subtype name matching

Be default names are matched with a `String` equality - `Subtype` would be considered a match for another `Subtype`
but not for `SUBTYPE` or any other capitalization.

The subtype name matching predicate can be overridden with a flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    sealed trait Foo
    object Foo {
      case object BAZ extends Foo
    }

    sealed trait Bar
    object Bar {
      case object Baz extends Bar
    }

    // (Foo.BAZ: Foo).transformInto[Bar] or
    // (Foo.BAZ: Foo).into[Bar].transform results in:
    // Chimney can't derive transformation from Foo to Bar
    //
    // Bar
    //   derivation from baz: Foo.BAZ to Bar is not supported in Chimney!
    //
    // Bar
    //   can't transform coproduct instance Foo.BAZ to Bar
    //
    // Consult https://chimney.readthedocs.io for usage examples.

    pprint.pprintln(
      (Foo.BAZ: Foo)
        .into[Bar]
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
    )
    pprint.pprintln(
      (Foo.BAZ: Foo)
        .intoPartial[Bar]
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
        .asEither
    )
    // expected output:
    // Baz
    // Right(value = Baz)

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)

      pprint.pprintln(
        (Foo.BAZ: Foo).transformInto[Bar]
      )
      pprint.pprintln(
        (Foo.BAZ: Foo).into[Bar].transform
      )
      // expected output:
      // Baz
      // Baz
      
      pprint.pprintln(
        (Foo.BAZ: Foo).transformIntoPartial[Bar].asEither
      )
      pprint.pprintln(
        (Foo.BAZ: Foo).intoPartial[Bar].transform.asEither
      )
      // expected output:
      // Right(value = Baz)
      // Right(value = Baz)
    }

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
      .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
      .buildTransformer
    ```

For details about `TransformedNamesComparison` look at [their dedicated section](#defining-custom-name-matching-predicate).

!!! warning

    Using a predicate can result in an ambiguity. It can usually be resolved by providing manual override for the ambiguous
    subtypes.

    An ambiguity can also appear, no matter which matcher is used, in cases like:

    ```scala
    // there are 2 "Name" subtypes!
    sealed trait Attribute
    object Attribute {
      object Person {
        case object FirstName extends Attribute
        case object LastName extends Attribute
        case object Address extends Attribute
      }

      object Company {
        case object Name extends Attribute
        case object Address extends Attribute
      }
    }
    ```

    Such cases always have to be handled manually (`.withSealedSubtypeHandled(...)`).

If the flag was enabled in the implicit config it can be disabled with `.disableCustomSubtypeNameComparison`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    sealed trait Foo
    object Foo {
      case object BAZ extends Foo
    }

    sealed trait Bar
    object Bar {
      case object Baz extends Bar
    }

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default
      .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)

    (Foo.BAZ: Foo).into[Bar].disableCustomSubtypeNameComparison.transform
    // expected error:
    // Chimney can't derive transformation from Foo to Bar
    //
    // Bar
    //   can't transform coproduct instance Foo.BAZ to Bar
    // Bar (transforming from: matching[Foo.BAZ])
    //   derivation from baz: Foo.BAZ to Bar is not supported in Chimney!
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

## From/into an `Option`

`Option` type has special support during the derivation of a transformation.

The transformation from one `Option` into another is obviously always supported:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String)
    case class Bar(a: String)

    pprint.pprintln(
      Option(Foo("value")).transformInto[Option[Bar]]
    )
    pprint.pprintln(
      (None: Option[Foo]).transformInto[Option[Bar]]
    )
    // expected output:
    // Some(value = Bar(a = "value"))
    // None
    
    pprint.pprintln(
      Option(Foo("value")).into[Option[Bar]].transform
    )
    pprint.pprintln(
      (None: Option[Foo]).into[Option[Bar]].transform
    )
    // expected output:
    // Some(value = Bar(a = "value"))
    // None
    
    pprint.pprintln(
      Option(Foo("value")).transformIntoPartial[Option[Bar]].asEither
    )
    pprint.pprintln(
      (None: Option[Foo]).transformIntoPartial[Option[Bar]].asEither
    )
    // expected output:
    // Right(value = Some(value = Bar(a = "value")))
    // Right(value = None)

    pprint.pprintln(
      Option(Foo("value")).intoPartial[Option[Bar]].transform.asEither
    )
    pprint.pprintln(
      (None: Option[Foo]).intoPartial[Option[Bar]].transform.asEither
    )
    // expected output:
    // Right(value = Some(value = Bar(a = "value")))
    // Right(value = None)

    import io.scalaland.chimney.{Transformer, PartialTransformer}

    // If we want to reuse Transformer, we can create implicits using:
    val totalTransformer: Transformer[Option[Foo], Option[Bar]] = Transformer.derive[Option[Foo], Option[Bar]]
    val partialTransformer: PartialTransformer[Option[Foo], Option[Bar]] = PartialTransformer.derive[Option[Foo], Option[Bar]]
    // or (if you want to pass overrides):
    val totalTransformer2: Transformer[Option[Foo], Option[Bar]] = Transformer.define[Option[Foo], Option[Bar]]
      .buildTransformer
    val partialTransformer2: PartialTransformer[Option[Foo], Option[Bar]] = PartialTransformer.define[Option[Foo], Option[Bar]]
      .buildTransformer
    ```

Additionally, an automatic wrapping with `Option` is also considered safe and always available:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String)
    case class Bar(a: String)

    pprint.pprintln(
      Foo("value").transformInto[Option[Bar]]
    )
    pprint.pprintln(
      Foo("value").into[Option[Bar]].transform
    )
    // expected output:
    // Some(value = Bar(a = "value"))
    // Some(value = Bar(a = "value"))

    pprint.pprintln(
      Foo("value").transformIntoPartial[Option[Bar]].asEither
    )
    pprint.pprintln(
      Foo("value").intoPartial[Option[Bar]].transform.asEither
    )
    // expected output:
    // Right(value = Some(value = Bar(a = "value")))
    // Right(value = Some(value = Bar(a = "value")))
    ```

However, unwrapping of an `Option` is impossible without handling `None` case, that's why Chimney handles it
automatically only with `PartialTransformer`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String)
    case class Bar(a: String)

    pprint.pprintln(
      Option(Foo("value"))
        .transformIntoPartial[Bar]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    pprint.pprintln(
      (None: Option[Foo])
        .transformIntoPartial[Bar]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = Bar(a = "value"))
    // Left(value = List(("", EmptyValue)))
    
    pprint.pprintln(
      Option(Foo("value"))
        .intoPartial[Bar]
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    pprint.pprintln(
      (None: Option[Foo])
        .intoPartial[Bar]
        .transform
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = Bar(a = "value"))
    // Left(value = List(("", EmptyValue)))
    ```

!!! tip

    Out of the box, Chimney supports only Scala's build-in `Option`s.
    
    If you need to integrate with Java's `Optional`, please, read about
    [Java's collections integration](cookbook.md#java-collections-integration).
    
    If you need to provide support for your optional types, please, read about
    [custom optional types](cookbook.md#custom-optional-types).

!!! tip

    You can use all the flags, renames, value provisions, and computations that are available to case classes,
    Java Beans and so on.

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String)
    case class Bar(a: String, b: String, c: Int, d: Char, e: Option[Float])

    pprint.pprintln(
      Option(Foo("value")).into[Option[Bar]]
        .withFieldRenamed(_.matchingSome.a, _.matchingSome.b)
        .withFieldConst(_.matchingSome.c, 10)
        .withFieldComputedFrom(_.matchingSome)(_.matchingSome.d, foo => foo.a.headOption.getOrElse('0'))
        .withTargetFlag(_.matchingSome.e).enableOptionDefaultsToNone
        .transform
    )
    // expected output:
    // Some(value = Bar(a = "value", b = "value", c = 10, d = 'v', e = None))
    ```

    While you could use `.matching[Some[Foo]].value` it is more convenient to use `.matchingSome` since it infers
    the inner type and exposes it automatically. Additionally, `.matchingSome` works with
    [custom optional types](cookbook.md#custom-optional-types).

### Controlling automatic `Option` unwrapping

Automatic unwrapping of `Option`s by `PartialTransformer`s allows for seamless decoding of many PTO types into domain
types and provides a nice symmetry with encoding values using `Transformer`s (wrapping values with `Option`).

However, sometimes you might prefer to opt out of such behavior. You can disable it with a flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: Option[Int])
    case class Bar(a: Int)

    Foo(Some(10)).intoPartial[Bar].disablePartialUnwrapsOption.transform.asOption
    // expected error:
    // Chimney can't derive transformation from Foo to Bar
    //
    // Bar
    //   a: scala.Int - can't derive transformation from a: scala.Option[scala.Int] in source type Foo
    //
    // scala.Int (transforming from: a into: a)
    //   derivation from foo.a: scala.Option[scala.Int] to scala.Int is not supported in Chimney!
    //
    // Consult https://chimney.readthedocs.io for usage examples.

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.disablePartialUnwrapsOption

      Foo(Some(10)).transformIntoPartial[Bar]
      // expected error:
      // Chimney can't derive transformation from Foo to Bar
      //
      // Bar
      //   a: scala.Int - can't derive transformation from a: scala.Option[scala.Int] in source type Foo
      //
      // scala.Int (transforming from: a into: a)
      //   derivation from foo.a: scala.Option[scala.Int] to scala.Int is not supported in Chimney!
      //
      // Consult https://chimney.readthedocs.io for usage examples.
      Foo(Some(10)).intoPartial[Bar].transform
      // expected error:
      // Chimney can't derive transformation from Foo to Bar
      //
      // Bar
      //   a: scala.Int - can't derive transformation from a: scala.Option[scala.Int] in source type Foo
      //
      // scala.Int (transforming from: a into: a)
      //   derivation from foo.a: scala.Option[scala.Int] to scala.Int is not supported in Chimney!
      //
      // Consult https://chimney.readthedocs.io for usage examples.
    }
    ```

If the flag was disabled in the implicit config it can be enabled with `.disablePartialUnwrapsOption`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: Option[Int])
    case class Bar(a: Int)

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.disablePartialUnwrapsOption

    pprint.pprintln(
      Foo(Some(10)).intoPartial[Bar].enablePartialUnwrapsOption.transform.asOption
    )
    pprint.pprintln(
      Foo(None).intoPartial[Bar].enablePartialUnwrapsOption.transform.asOption
    )
    // expected output:
    // Some(value = Bar(a = 10))
    // None
    ```

## Between `Either`s

A transformation from one `Either` to another is supported as long as both left and right types can also be converted:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String)
    case class Bar(a: String)

    pprint.pprintln(
      (Left(Foo("value")): Either[Foo, Bar]).transformInto[Either[Bar, Foo]]
    )
    pprint.pprintln(
      (Right(Bar("value")): Either[Foo, Bar]).transformInto[Either[Bar, Foo]]
    )
    // expected output:
    // Left(value = Bar(a = "value"))
    // Right(value = Foo(a = "value"))

    pprint.pprintln(
      (Left(Foo("value")): Either[Foo, Bar]).transformIntoPartial[Either[Bar, Foo]].asOption
    )
    pprint.pprintln(
      (Right(Bar("value")): Either[Foo, Bar]).transformIntoPartial[Either[Bar, Foo]].asOption
    )
    // expected output:
    // Some(value = Left(value = Bar(a = "value")))
    // Some(value = Right(value = Foo(a = "value")))

    import io.scalaland.chimney.Transformer

    // If we want to reuse Transformer, we can create implicits using:
    val transformer: Transformer[Either[Foo, Bar], Either[Bar, Foo]] = Transformer.derive[Either[Foo, Bar], Either[Bar, Foo]]

    import io.scalaland.chimney.{Transformer, PartialTransformer}

    // If we want to reuse Transformer, we can create implicits using:
    val totalTransformer: Transformer[Either[Foo, Bar], Either[Bar, Foo]] = Transformer.derive[Either[Foo, Bar], Either[Bar, Foo]]
    val partialTransformer: PartialTransformer[Either[Foo, Bar], Either[Bar, Foo]] = PartialTransformer.derive[Either[Foo, Bar], Either[Bar, Foo]]
    // or (if you want to pass overrides):
    val totalTransformer2: Transformer[Either[Foo, Bar], Either[Bar, Foo]] = Transformer.define[Either[Foo, Bar], Either[Bar, Foo]]
      .buildTransformer
    val partialTransformer2: PartialTransformer[Either[Foo, Bar], Either[Bar, Foo]] = PartialTransformer.define[Either[Foo, Bar], Either[Bar, Foo]]
      .buildTransformer
    ```

A transformation from `Left` and `Right` into `Either` requires existence of only the transformation from the type we
know for sure is inside to their corresponding type in target `Either`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: Int)
    case class Bar(a: String)
    case class Baz(a: String, b: Int, c: Long)

    // Foo -> Bar - can be derived
    // Foo -> Baz - cannot be derived without providing c
    pprint.pprintln(
      (Left(Foo("value", 10))).transformInto[Either[Bar, Baz]]
    )
    pprint.pprintln(
      (Right(Foo("value", 10))).transformInto[Either[Baz, Bar]]
    )
    // expected output:
    // Left(value = Bar(a = "value"))
    // Right(value = Bar(a = "value"))
    ```

!!! tip

    You can use all the flags, renames, value provisions, and computations that are available to case classes,
    Java Beans and so on.

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a0: String)
    case class Bar(a: String, b: Int, c: Char, d: Option[Float])

    pprint.pprintln(
      (Right(Foo("value")): Either[Bar, Foo]).into[Either[Foo, Bar]]
      .withFieldRenamed(_.matchingLeft.a, _.matchingLeft.a0)
      .withFieldRenamed(_.matchingRight.a0, _.matchingRight.a)
      .withFieldConst(_.matchingRight.b, 10)
      .withFieldComputedFrom(_.matchingRight)(_.matchingRight.c, bar => bar.a0.headOption.getOrElse('0'))
      .withTargetFlag(_.matchingRight.d).enableOptionDefaultsToNone
      .transform
    )
    // expected output:
    // Right(value = Bar(a = "value", b = 10, c = 'v', d = None))
    ```

    While you could use `.matching[Left[Foo]].value`/`.matching[Right[Bar]].value` it is more convenient to use
    `.matchingLeft`/`.matchingRight` since it infers the inner type and exposes it automatically.

## Between Scala's collections/`Array`s

Every `Array`/every collection extending `scala.collection.Iterable` can be used as a source value for a collection's
transformation.

Every `Array`/every collection provided with `scala.collection.compat.Factory` can be used as a target type for a
collection's transformation.

The requirement for a collection's transformation is that both source's and target's conditions are met and that
the types stored within these collections can also be converted. 

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    import scala.collection.immutable.ListMap

    case class Foo(a: String)
    case class Bar(a: Option[String])

    pprint.pprintln(
      List(Foo("value")).transformInto[Vector[Bar]]
    )
    pprint.pprintln(
      Map(Foo("key") -> Foo("value")).transformInto[Array[(Bar, Bar)]]
    )
    pprint.pprintln(
      Vector(Foo("key") -> Foo("value")).transformInto[ListMap[Bar, Bar]]
    )
    // expected output:
    // Vector(Bar(a = Some(value = "value")))
    // Array((Bar(a = Some(value = "key")), Bar(a = Some(value = "value"))))
    // ListMap(Bar(a = Some(value = "key")) -> Bar(a = Some(value = "value")))

    import io.scalaland.chimney.{Transformer, PartialTransformer}

    // If we want to reuse Transformer, we can create implicits using:
    val totalTransformer: Transformer[List[Foo], Vector[Bar]] = Transformer.derive[List[Foo], Vector[Bar]]
    val partialTransformer: PartialTransformer[List[Foo], Vector[Bar]] = PartialTransformer.derive[List[Foo], Vector[Bar]]
    // or (if you want to pass overrides):
    val totalTransformer2: Transformer[List[Foo], Vector[Bar]] = Transformer.define[List[Foo], Vector[Bar]]
      .buildTransformer
    val partialTransformer2: PartialTransformer[List[Foo], Vector[Bar]] = PartialTransformer.define[List[Foo], Vector[Bar]]
      .buildTransformer
    ```

With `PartialTransformer`s ware able to handle fallible conversions, tracing at which key/index the failure occurred:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String)
    case class Bar(a: Option[String])

    pprint.pprintln(
      List(Bar(Some("value")), Bar(None))
        .transformIntoPartial[Vector[Foo]]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    pprint.pprintln(
      Map(Bar(Some("value")) -> Bar(None), Bar(None) -> Bar(Some("value")))
        .transformIntoPartial[Vector[(Foo, Foo)]]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Left(value = List(("(1).a", EmptyValue)))
    // Left(value = List(("(Bar(Some(value))).a", EmptyValue), ("keys(Bar(None)).a", EmptyValue)))
    ```

!!! tip

    Out of the box, Chimney supports only Scala's build-in collections, which are extending `Iterable` and have
    `scala.collection.compat.Factory` provided as an implicit.
    
    If you need to integrate with Java's collections, please, read about
    [Java's collections integration](cookbook.md#java-collections-integration).
    
    If you need to provide support for your collection types, you have to write your own implicit methods.

!!! tip

    You can use all the flags, renames, value provisions, and computations that are available to case classes,
    Java Beans and so on.

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String)
    case class Bar(a: String, b: String, c: Int, d: Char, e: Option[Float])

    pprint.pprintln(
      List(Foo("key") -> Foo("value")).into[Map[Bar, Bar]]
        .withFieldRenamed(_.everyItem._1.a, _.everyMapKey.b)
        .withFieldConst(_.everyMapKey.c, 10)
        .withFieldComputedFrom(_.everyItem._1)(_.everyMapKey.d, foo => foo.a.headOption.getOrElse('0'))
        .withTargetFlag(_.everyMapKey.e).enableOptionDefaultsToNone
        .withFieldRenamed(_.everyItem._2.a, _.everyMapValue.b)
        .withFieldConst(_.everyMapValue.c, 10)
        .withFieldComputedFrom(_.everyItem._2)(_.everyMapValue.d, foo => foo.a.headOption.getOrElse('0'))
        .withTargetFlag(_.everyMapValue.e).enableOptionDefaultsToNone
        .transform
    )
    // expected output:
    // Map(
    //   Bar(a = "key", b = "key", c = 10, d = 'k', e = None) -> Bar(a = "value", b = "value", c = 10, d = 'v', e = None)
    // )
    ```

    `.everyItem`/`.everyMapKey`/`.everyMapValue` work with [custom optional types](cookbook.md#custom-collection-types).

## Parametric types/generics

The Transformation from/to the parametric type can always be derived, when Chimney know how to transform each value
defined with a type parameter.

The most obvious case is having all type parameters applied to non-abstract types:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo[A](value: A)
    case class Bar[A](value: A)

    case class Baz[A](value: A)

    pprint.pprintln(
      Foo(Baz("value")).transformInto[Bar[Baz[String]]]
    )
    // expected output:
    // Bar(value = Baz(value = "value"))

    import io.scalaland.chimney.{Transformer, PartialTransformer}

    // If we want to reuse Transformer, we can create implicits using:
    val totalTransformer: Transformer[Foo[Baz[String]], Bar[Bar[String]]] = Transformer.derive[Foo[Baz[String]], Bar[Bar[String]]]
    val partialTransformer: PartialTransformer[Foo[Baz[String]], Bar[Bar[String]]] = PartialTransformer.derive[Foo[Baz[String]], Bar[Bar[String]]]
    // or (if you want to pass overrides):
    val totalTransformer2: Transformer[Foo[Baz[String]], Bar[Bar[String]]] = Transformer.define[Foo[Baz[String]], Bar[Bar[String]]]
      .buildTransformer
    val partialTransformer2: PartialTransformer[Foo[Baz[String]], Bar[Bar[String]]] = PartialTransformer.define[Foo[Baz[String]], Bar[Bar[String]]]
      .buildTransformer
    ```

or having type parameter being not used at all:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    type AbstractType1
    type AbstractType2

    case class Foo[A](value: String)
    case class Bar[A](value: String)

    pprint.pprintln(
      Foo[AbstractType1]("value").transformInto[Bar[AbstractType2]]
    )
    // expected output:
    // Bar(value = "value")
    ```

If the type is `abstract` and used as a value, but contains enough information that one of existing rules
knows how to apply it, the transformation can still be derived:

!!! example
 
    If Chimney knows that type can be safely upcasted, the upcasting is available to it:

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo[A](value: A)
    case class Bar[A](value: A)

    def upcastingExample[A, B >: A](foo: Foo[A]): Bar[B] =
      foo.transformInto[Bar[B]]

    pprint.pprintln(
      upcastingExample[Int, AnyVal](Foo(10))
    )
    // expected output:
    // Bar(value = 10)
    ```
    
    If we don't know the exact type but we know enough to read the relevant fields, we can also do it:

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    trait Baz[A] { val value: A }
    case class Foo[A](value: A) extends Baz[A]
    case class Bar[A](value: A)

    def subtypeExample[A <: Baz[String]](foo: Foo[A]): Bar[Bar[String]] =
      foo.transformInto[Bar[Bar[String]]]

    pprint.pprintln(
      subtypeExample(Foo(Foo("value")))
    )
    // expected output:
    // Bar(value = Bar(value = "value"))
    ```
    
    On Scala 2, we are even able to use refined types (Scala 3, changed a bit how they works):

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo[A](value: A)
    case class Bar[A](value: A)

    def refinedExample[A <: { val value: String }](foo: Foo[A]): Bar[Bar[String]] =
      foo.transformInto[Bar[Bar[String]]]

    pprint.pprintln(
      refinedExample[Foo[String]](Foo(Foo("value")))
    )
    // expected output:
    // Bar(value = Bar(value = "value"))
    ```

Finally, you can always provide a custom `Transformer` from/to a type containing a type parameter, as an `implicit`:

!!! example
 
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.Transformer

    case class Foo[A](value: A)
    case class Bar[A](value: A)

    def conversion[A, B](foo: Foo[A])(implicit transformer: Transformer[A, B]): Bar[B] =
      foo.transformInto[Bar[B]]
    ```

!!! tip

    For more information about defining custom `Transformer`s and `PartialTransformer`s, you read the section below.
    
    If you need to fetch and pass around implicit transformers (both total and partial), read
    the [Automatic, semiautomatic and inlined derication](cookbook.md#automatic-semiautomatic-and-inlined-derivation)
    cookbook.

## Into singleton types

If the target is one of supported singleton types, we can provide the transformation based only on the type.

### Into a literal-based singleton type

Scala 2.13 and 3 allow using [literal-based singleton types](https://docs.scala-lang.org/sips/42.type.html):

!!! example
 
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    
    case class Example(a: Int, b: String)

    pprint.pprintln(
      Example(10, "test").transformInto[true]
    )
    pprint.pprintln(
      Example(10, "test").into[true].transform
    )
    // expected output:
    // true
    // true
    
    pprint.pprintln(
      Example(10, "test").transformInto[1024]
    )
    pprint.pprintln(
      Example(10, "test").into[1024].transform
    )
    // expected output:
    // 1024
    // 1024
    
    pprint.pprintln(
      Example(10, "test").transformInto[1024L]
    )
    pprint.pprintln(
      Example(10, "test").into[1024L].transform
    )
    // expected output:
    // 1024L
    // 1024L
    
    pprint.pprintln(
      Example(10, "test").transformInto[3.14f]
    )
    pprint.pprintln(
      Example(10, "test").into[3.14f].transform
    )
    // expected output:
    // 3.14F
    // 3.14F
    
    pprint.pprintln(
      Example(10, "test").transformInto[3.14]
    )
    pprint.pprintln(
      Example(10, "test").into[3.14].transform
    )
    // expected output:
    // 3.14
    // 3.14
    
    pprint.pprintln(
      Example(10, "test").transformInto['@']
    )
    pprint.pprintln(
      Example(10, "test").into['@'].transform
    )
    // expected output:
    // '@'
    // '@'
    
    pprint.pprintln(
      Example(10, "test").transformInto["str"]
    )
    pprint.pprintln(
      Example(10, "test").into["str"].transform
    )
    // expected output:
    // "str"
    // "str"
    ```

### Into a case object

When the target is a `case object`, the transformation can always be provided:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Example(a: Int, b: String)
    case object SomeObject

    pprint.pprintln(
      Example(10, "test").transformInto[SomeObject.type]
    )
    pprint.pprintln(
      Example(10, "test").into[SomeObject.type].transform
    )
    // expected output:
    // SomeObject
    // SomeObject
    
    pprint.pprintln(
      Example(10, "test").transformIntoPartial[SomeObject.type].asEither
    )
    pprint.pprintln(
      Example(10, "test").intoPartial[SomeObject.type].transform.asEither
    )
    // expected output:
    // Right(value = SomeObject)
    // Right(value = SomeObject)
    ```

On Scala 3, parameterless `case` can be used as well:    

!!! example

    ```scala
    // file: snippet.scala - part of example of parameterless case as target type
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Example(a: Int, b: String)
    enum SomeEnum:
      case SomeValue

    @main def examples: Unit = {
    pprint.pprintln(
      Example(10, "test").transformInto[SomeEnum.SomeValue.type]
    )
    pprint.pprintln(
      Example(10, "test").into[SomeEnum.SomeValue.type].transform
    )
    // expected output:
    // SomeValue
    // SomeValue
    
    pprint.pprintln(
      Example(10, "test").transformIntoPartial[SomeEnum.SomeValue.type].asEither
    )
    pprint.pprintln(
      Example(10, "test").intoPartial[SomeEnum.SomeValue.type].transform.asEither
    )
    // expected output:
    // Right(value = SomeValue)
    // Right(value = SomeValue)
    }
    ```

!!! notice

    `Unit` and `None.type` are explicitly excluded for safety reasons. If you want to enable conversion into them,
     provide an implicit manually.

## Types with manually provided constructors

If you cannot use a public primary constructor to create the target type, it is NOT a Scala collection, `Option`,
`AnyVal`, ... but is e.g.:

  - a type using a smart constructor
  - a type which has multiple constructors and you need to point which one you want to use
  - abstract type defined next to an abstract method that will instantiate it
  - non-`sealed` `trait` where you want to pick one particular implementation for your transformation

AND you do know a way of constructing this type using a method - or handwritten lambda - you can point to that method.
Then Chimney will try to match the source type's getters against the method's parameters by their names:  

!!! example
 
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(value: Int)
    case class Bar private (value: String)
    object Bar {
      def make(value: Int): Bar = Bar(value.toString)
    }

    pprint.pprintln(
      Foo(10).into[Bar].withConstructor(Bar.make _).transform
    )
    // expected output:
    // Bar(value = "10")

    pprint.pprintln(
      Foo(10)
        .into[Bar]
        .withConstructor { (value: Int) =>
          Bar.make(value * 100)
        }
        .transform
    )
    // expected output:
    // Bar(value = "1000")
    
    // we can also provide constructor to selected fields
    pprint.pprintln(
      List(Foo(10))
        .into[Vector[Bar]]
        .withConstructorTo(_.everyItem) { (value: Int) =>
          Bar.make(value * 100)
        }
        .transform
    )
    // expected output:
    // Vector(Bar(value = "1000"))

    import io.scalaland.chimney.{Transformer, PartialTransformer}

    // If we want to reuse Transformer, we can create implicits using:
    val totalTransformer: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
      .withConstructor { (value: Int) =>
        Bar.make(value * 100)
      }
      .buildTransformer
    val partialTransformer: PartialTransformer[Foo, Bar] = PartialTransformer.define[Foo, Bar]
      .withConstructor { (value: Int) =>
        Bar.make(value * 100)
      }
      .buildTransformer
    ```

!!! note

    `.withConstructor` overrides the constructor **only on the top level target**. It would not be used when the same type
    occurs somewhere in a nested field.

    Similarly `.withConstructorTo` only overrides the constructor for the selected field.

!!! warning

    The current implementation has a limit of 22 arguments even on Scala 3 (it doesn't use `scala.FunctionXXL`).
    
    It also requires that you either pass a method (which will be Eta-expanded) or a lambda with _all_ parameters names
    (to allow matching parameters by name). It allows the method to have multiple parameters list and lambda to be
    defined as curried (`(a: A, b: B) => (c: C) => { ... }`). 

If your type only has smart a constructor which e.g. validates the input and might fail, you can provide a that smart
constructor for `PartialTransformer`:

!!! example
 
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    case class Foo(value: String)
    case class Bar private (value: Int)
    object Bar {
      def parse(value: String): Either[String, Bar] =
        scala.util.Try(value.toInt).toEither.map(new Bar(_)).left.map(_.getMessage)
    }

    def smartConstructor(value: String): partial.Result[Bar] =
      partial.Result.fromEitherString(Bar.parse(value))

    pprint.pprintln(
      Foo("10")
        .intoPartial[Bar]
        .withConstructorPartial(smartConstructor _)
        .transform
        .asEither
    )
    // expected output:
    // Right(value = Bar(value = 10))

    pprint.pprintln(
      Foo("10")
        .intoPartial[Bar]
        .withConstructorPartial { (value: String) =>
          partial.Result.fromEitherString(Bar.parse(value))
        }
        .transform
        .asEither
    )
    // expected output:
    // Right(value = Bar(value = 10))
      
    // or even shorted if your smart constructor uses Either[String, YourType]
    
    pprint.pprintln(
      Foo("10")
        .intoPartial[Bar]
        .withConstructorEither(Bar.parse _)
        .transform
        .asEither
    )
    // expected output:
    // Right(value = Bar(value = 10))
    
    // we can also provide constructor to selected fields
    
    pprint.pprintln(
      List(Foo("10"))
        .intoPartial[Vector[Bar]]
        .withConstructorPartialTo(_.everyItem)(smartConstructor _)
        .transform
        .asEither
    )
    // expected output:
    // Right(value = Vector(Bar(value = 10)))
    
    pprint.pprintln(
      List(Foo("10"))
        .intoPartial[Vector[Bar]]
        .withConstructorEitherTo(_.everyItem)(Bar.parse _)
        .transform
        .asEither
    )
    // expected output:
    // Right(value = Vector(Bar(value = 10)))
    ```

You can use this to automatically match the source's getters e.g. against smart constructor's arguments - these types
would almost always have methods which the user could recognize as constructor's but which might be difficult
to be automatically recognized as such: 

!!! example
 
    Due to the nature of `opaque type`s this example needs to have opaque types defined in a different `.scala` file
    than where they are being used:

    ```scala
    // file: models.scala - part of opaque example
    package models

    case class StringIP(s1: String, s2: String, s3: String, s4: String)

    opaque type IP = Int
    extension (ip: IP)
      def _1: Byte = ((ip >> 24) & 255).toByte
      def _2: Byte = ((ip >> 16) & 255).toByte
      def _3: Byte = ((ip >> 8) & 255).toByte
      def _4: Byte = ((ip >> 0) & 255).toByte
      def value: Int = ip
      def show: String = s"${_1}.${_2}.${_3}.${_4}"
    object IP {
      def parse(s1: String, s2: String, s3: String, s4: String): Either[String, IP] =
        scala.util
          .Try {
            val i1 = (s1.toInt & 255) << 24
            val i2 = (s2.toInt & 255) << 16
            val i3 = (s3.toInt & 255) << 8
            val i4 = (s4.toInt & 255)
            i1 + i2 + i3 + i4
          }
          .toEither
          .left
          .map(_.getMessage)
    }
    ```

    ```scala
    // file: main.scala - part of opaque example
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    package example

    import io.scalaland.chimney.dsl.*
    import io.scalaland.chimney.{partial, PartialTransformer}
    import models.*

    given PartialTransformer[StringIP, IP] = PartialTransformer
      .define[StringIP, IP]
      .withConstructorPartial { (s1: String, s2: String, s3: String, s4: String) =>
        partial.Result.fromEitherString(IP.parse(s1, s2, s3, s4))
      }
      .buildTransformer
      
    // or:
    
    // given PartialTransformer[StringIP, IP] = PartialTransformer
    //  .define[StringIP, IP]
    //  .withConstructorEither(IP.parse)
    //  .buildTransformer

    @main def example: Unit = {
    pprint.pprintln(
      StringIP("127", "0", "0", "1").transformIntoPartial[IP].asEither.map(_.show)
    )
    // expected output:
    // Right(value = "127.0.0.1")
    }
    ```

!!! example

    ```scala
    // file: models.scala - part of opaque example 2
    package models

    case class Foo(value: String)

    opaque type Bar = Int
    extension (bar: Bar) def value: Int = bar
    object Bar {
      def parse(value: String): Either[String, Bar] =
        scala.util.Try(value.toInt).toEither.left.map(_.getMessage)
    }
    ```

    ```scala
    // file: main.scala - part of opaque example 2
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    package example

    import io.scalaland.chimney.dsl.*
    import io.scalaland.chimney.{partial, PartialTransformer}
    import models.{Bar, Foo}

    given PartialTransformer[Foo, Bar] = PartialTransformer
      .define[Foo, Bar]
      .withConstructorPartial { (value: String) =>
        partial.Result.fromEitherString(Bar.parse(value))
      }
      .buildTransformer
      
    // or:

    // given PartialTransformer[Foo, Bar] = PartialTransformer
    //  .define[Foo, Bar]
    //  .withConstructorEither(Bar.parse(value))
    //  .buildTransformer

    @main def example: Unit = {
    pprint.pprintln(
      Foo("10").transformIntoPartial[Bar].asEither
    )
    // expected output:
    // Right(value = 10)
    }
    ```

!!! tip 

    `opaque type`s usually have only one constructor argument, and usually it is easier to not transform them that way,
    but rather call their constructor directly. If `opaque type`s are nested in the transformed structure, it might be
    easier to define [a custom transformer](#custom-transformations), perhaps by using a dedicated new type/refined type
    library and [providing an integration for all of its types](cookbook.md#libraries-with-smart-constructors).

## Merging multiple input sources into a single target value

Sometimes we have one flat structure on one side and several structures on the other.

```scala
import java.time.Instant

case class UserDto(id: Long, firstName: String, lastName: String, createdAt: Instance, updatedAt: Instant)

case class User(id: Long, data: Data, meta: Meta)
object User {
  case class Data(firstName: String, lastName: String)
  case class Meta(createdAt: Instance, updatedAt: Instant)
}
```

We could easily convert a whole model into smaller pieces:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}    
    import io.scalaland.chimney.dsl._
    import java.time.Instant

    case class UserDto(id: Long, firstName: String, lastName: String, createdAt: Instant, updatedAt: Instant)

    case class User(id: User.ID, data: User.Data, meta: User.Meta)
    object User {
      case class ID(id: Long)
      case class Data(firstName: String, lastName: String)
      case class Meta(createdAt: Instant, updatedAt: Instant)
    }

    val dto = UserDto(42L, "John", "Smith", Instant.parse("2000-01-01T12:00:00.00Z"), Instant.parse("2010-01-01T12:00:00.00Z"))

    val id   = dto.transformInto[User.ID]
    val data = dto.transformInto[User.Data]
    val meta = dto.transformInto[User.Meta]
    pprint.pprintln(
      User(id, data, meta)
    )
    // expected output:
    // User(
    //   id = ID(id = 42L),
    //   data = Data(firstName = "John", lastName = "Smith"),
    //   meta = Meta(createdAt = 2000-01-01T12:00:00Z, updatedAt = 2010-01-01T12:00:00Z)
    // )
    ```

but how could we combine them back together without wiring most of the fields manually?

By defining some source value as the main one (usable in DSL for e.g. renames), and 1 or more fallbacks:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    import java.time.Instant

    case class UserDto(id: Long, firstName: String, lastName: String, createdAt: Instant, updatedAt: Instant)

    case class User(id: User.ID, data: User.Data, meta: User.Meta)
    object User {
      case class ID(id: Long)
      case class Data(firstName: String, lastName: String)
      case class Meta(createdAt: Instant, updatedAt: Instant)
    }

    val user = User(
      User.ID(42L),
      User.Data("John", "Smith"),
      User.Meta(Instant.parse("2000-01-01T12:00:00.00Z"), Instant.parse("2010-01-01T12:00:00.00Z"))
    )
    pprint.pprintln(
      user.id.into[UserDto]
        .withFallback(user.data)
        .withFallback(user.meta)
        .transform
    )
    // expected output:
    // UserDto(
    //   id = 42L,
    //   firstName = "John",
    //   lastName = "Smith",
    //   createdAt = 2000-01-01T12:00:00Z,
    //   updatedAt = 2010-01-01T12:00:00Z
    // )
    ```

  Sometimes we might want to provide fallbacks only to some fields rather than globally:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    import java.time.Instant

    case class UserDto(id: Long, firstName: String, lastName: String, createdAt: Instant, updatedAt: Instant)

    case class User(id: User.ID, data: User.Data, meta: User.Meta)
    object User {
      case class ID(id: Long)
      case class Data(firstName: String, lastName: String)
      case class Meta(createdAt: Instant, updatedAt: Instant)
    }

    case class Nested[A](value: A)

    val user = User(
      User.ID(42L),
      User.Data("John", "Smith"),
      User.Meta(Instant.parse("2000-01-01T12:00:00.00Z"), Instant.parse("2010-01-01T12:00:00.00Z"))
    )
    pprint.pprintln(
      Nested(User.ID(128L)).into[Nested[UserDto]]
        .withFallbackFrom(_.value)(user.data)
        .withFallbackFrom(_.value)(user.meta)
        .transform
    )
    // expected output:
    // Nested(
    //   value = UserDto(
    //     id = 128L,
    //     firstName = "John",
    //     lastName = "Smith",
    //     createdAt = 2000-01-01T12:00:00Z,
    //     updatedAt = 2010-01-01T12:00:00Z
    //   )
    // )
    ```

!!! warning

    If the source type `<:<` target type, [subtyping rule will apply](#upcasting-and-identity-transformation) and merging will not happen.

### Merging `case class`es (or POJOs)

This is how the merging algorithm works:

 * when mergnig several `case class`es (or POJOs) into a `case class` (or POJO), for each target field:
   * check if there is explicitly provided override ([const value](#wiring-the-constructors-parameter-to-a-provided-value),
     [computed value](#wiring-the-constructors-parameter-to-the-computed-value),
     [rename](#wiring-the-constructors-parameter-to-its-source-field), ...)
     * if there is one, use it
   * if there is no override, see if the source value try to find a field with a matching name
     * if there is one, convert it into the target field's type
     * use fallbacks' matching fields as the source field's fallbacks
   * if there is no source field, go through the list of fallback values (in the order they were provided) and check
     if one of them has a field with a matching name
     * if there is one, convert it into the target field's type
   * if there is no such field, try to use other fallback values if they are enabled
     ([default values](#allowing-fallback-to-the-constructors-default-values),
     [`None`](#allowing-fallback-to-none-as-the-constructors-argument)) as last resort

!!! tip

    You can use all overrides and flags that you normally use for [a single value conversions](#into-a-case-class-or-pojo).

!!! tip

    You can control priority of fallback values just by ordering them:

    ```scala
    theMostImprtant.into[Result].withFallback(lessImportant).withFallback(evenLessImportant) ...
    ```

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: Int, b: String)
    case class Bar(b: String, c: Long)
    case class Baz(c: Long, d: String)
    
    case class Result(a: Int, b: String, c: Long, d: String, e: Int)

    pprint.pprintln(
      Foo(1, "2").into[Result]
        .withFallback(Bar("3", 4L))
        .withFallback(Baz(5L, "6"))
        .withFieldConst(_.e, 7)
        .transform
    )
    // expected output:
    // Result(a = 1, b = "2", c = 4L, d = "6", e = 7)
    ```

!!! tip

    `case class`es/POJOs are merged **recursively** - matching fallbacks' fields becomes fallbacks themselves automatically

!!! example s

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Source1(value: Source2)
    case class Source2(innerValue: Source3)
    case class Source3(foo: String)

    case class Fallback1(value: Fallback2)
    case class Fallback2(innerValue: Fallback3)
    case class Fallback3(bar: String)

    case class Target1(value: Target2)
    case class Target2(innerValue: Target3)
    case class Target3(foo: String, bar: String)

    pprint.pprintln(
      Source1(Source2(Source3("value 1"))).into[Target1]
        .withFallback(Fallback1(Fallback2(Fallback3("value 2"))))
        .transform
    )
    // expected output:
    // Target1(value = Target2(innerValue = Target3(foo = "value 1", bar = "value 2")))
    ```

### Merging with at least one tuple

If there is at least 1 tuple-type among: source value type, target type, fallbacks, then merging works as following:

 * take all `val`s from the source as list
 * take all `val`s from 1st fallback as list
 * ...
 * concatenate these lists
 * fill target type's values with (converted) values by their **index**

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: Int)
    case class Bar(c: String, d: Int)
    case class Baz(a: String, b: Int, c: String, d: Int)

    pprint.pprintln(
      Foo("1", 2).into[(String, Int, String, Int)]
        .withFallback(Bar("3", 4))
        .transform
    )
    // expected output:
    // ("1", 2, "3", 4)

    pprint.pprintln(
      Foo("1", 2).into[Baz]
        .withFallback(("3", 4))
        .transform
    )
    // expected output:
    // Baz(a = "1", b = 2, c = "3", d = 4)

    pprint.pprintln(
      ("1", 2).into[Baz]
        .withFallback(Bar("3", 4))
        .transform
    )
    // expected output:
    // Baz(a = "1", b = 2, c = "3", d = 4)
    ```

### Merging `AnyVal`s

If the source type is `AnyVal` (or wrapper [if they are enabled](#frominto-a-wrapper-type)), both it and fallbacks can
be automaically unwrapped:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class ValueType[A](value: A) extends AnyVal
    case class Foo(a: String)
    case class Bar(b: String)
    case class Baz(a: String, b: String)

    pprint.pprintln(
      ValueType(Foo("a")).into[Baz].withFallback(Bar("b")).transform
    )
    // expected output:
    // Baz(a = "a", b = "b")

    pprint.pprintln(
      ValueType(Foo("a")).into[ValueType[Baz]].withFallback(Bar("b")).transform
    )
    // expected output:
    // ValueType(value = Baz(a = "a", b = "b"))
    
    pprint.pprintln(
      ValueType(Foo("a")).into[Baz].withFallback(ValueType(Bar("b"))).transform
    )
    // expected output:
    // Baz(a = "a", b = "b")
    
    pprint.pprintln(
      ValueType(Foo("a")).into[ValueType[Baz]].withFallback(ValueType(Bar("b"))).transform
    )
    // expected output:
    // ValueType(value = Baz(a = "a", b = "b"))
    ```

### Merging `sealed`/`enum` with `case class`/POJO into `sealed`/`enum`

It is possible to convert each subtype/case of a source `sealed`/`enum` into corresponding 
subtype/case of a target `sealed`/`enum` sharing the same common fallbacks between them:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    sealed trait Foo extends Product with Serializable
    object Foo {
      case class One(a: Int) extends Foo
      case class Two(b: String) extends Foo
    }

    case class Bar(c: Boolean)

    sealed trait Baz extends Product with Serializable
    object Baz {
      case class One(a: Int, c: Boolean) extends Baz
      case class Two(b: String, c: Boolean) extends Baz
    }

    pprint.pprintln(
      (Foo.One(10): Foo).into[Baz].withFallback(Bar(true)).transform
    )
    // expected output:
    // One(a = 10, c = true)
    pprint.pprintln(
      (Foo.Two("test"): Foo).into[Baz].withFallback(Bar(true)).transform
    )
    // expected output:
    // Two(b = "test", c = true)
    ```


### Merging `Option` with `Option` into `Option`

If we have:

 * `Option`-type in the source type/field
 * `Option`-type in the target type/field
 * 1 or more `Option`-types in fallbacks

there are 3 possible ways of handling them with Chimney:

 * just take the source value's value and call it a day (default)
 * merge left-to-right (`src.orElse(fallback2).orElse(fallback2)...`)
 * merge right-to-left (`fallback2.orElse(fallback1).orElse(src)...`)

We can select merging with `.enableOptionFallbackMerge` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(value: String)
    case class Bar(value: String)
    case class Baz(value: String)

    pprint.pprintln(
      Option.empty[Foo].into[Option[Bar]]
        .withFallback(Option(Foo("fallback1")))
        .withFallback(Option(Baz("fallback2")))
        .transform
    )
    // expected output:
    // None

    pprint.pprintln(
      Option.empty[Foo].into[Option[Bar]]
        .withFallback(Option(Foo("fallback1")))
        .withFallback(Option(Baz("fallback2")))
        .enableOptionFallbackMerge(SourceOrElseFallback)
        .transform
    )
    // expected output:
    // Some(value = Bar(value = "fallback1"))

    pprint.pprintln(
      Option.empty[Foo].into[Option[Bar]]
        .withFallback(Option(Foo("fallback1")))
        .withFallback(Option(Baz("fallback2")))
        .enableOptionFallbackMerge(FallbackOrElseSource)
        .transform
    )
    // expected output:
    // Some(value = Bar(value = "fallback2"))
    ```

### Merging `Either` with `Either` into `Either`

If we have:

 * `Either`-type in the source type/field
 * `Either`-type in the target type/field
 * 1 or more `Either`-types in fallbacks

there are 3 possible ways of handling them with Chimney:

 * just take the source value's value and call it a day (default)
 * merge left-to-right (`src.orElse(fallback2).orElse(fallback2)...`)
 * merge right-to-left (`fallback2.orElse(fallback1).orElse(src)...`)

We can select merging with `.enableEitherFallbackMerge` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(value: String)
    case class Bar(value: String)
    case class Baz(value: String)

    pprint.pprintln(
      (Left("nope"): Either[String, Foo]).into[Either[String, Bar]]
        .withFallback(Right(Foo("fallback1")): Either[String, Foo])
        .withFallback(Right(Baz("fallback2")): Either[String, Baz])
        .transform
    )
    // expected output:
    // Left(value = "nope")

    pprint.pprintln(
      (Left("nope"): Either[String, Foo]).into[Either[String, Bar]]
        .withFallback(Right(Foo("fallback1")): Either[String, Foo])
        .withFallback(Right(Baz("fallback2")): Either[String, Baz])
        .enableEitherFallbackMerge(SourceOrElseFallback)
        .transform
    )
    // expected output:
    // Right(value = Bar(value = "fallback1"))

    pprint.pprintln(
      (Left("nope"): Either[String, Foo]).into[Either[String, Bar]]
        .withFallback(Right(Foo("fallback1")): Either[String, Foo])
        .withFallback(Right(Baz("fallback2")): Either[String, Baz])
        .enableEitherFallbackMerge(FallbackOrElseSource)
        .transform
    )
    // expected output:
    // Right(value = Bar(value = "fallback2"))
    ```

### Merging collection with collection into collection

If we have:

 * collection-type in the source type/field
 * collection-type in the target type/field
 * 1 or more collections in fallbacks

there are 3 possible ways of handling them with Chimney:

 * just take the source value's value and call it a day (default)
 * merge left-to-right (`src ++ fallback2 ++ fallback2...`)
 * merge right-to-left (`fallback2 ++ fallback1 ++ src...`)

We can select merging with `.enableCollectionFallbackMerge` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(value: String)
    case class Bar(value: String)
    case class Baz(value: String)

    pprint.pprintln(
      (List(Foo("source"))).into[Vector[Bar]]
        .withFallback(Array(Foo("fallback1")))
        .withFallback(Set(Baz("fallback2")))
        .transform
    )
    // expected output:
    // Vector(Bar(value = "source"))

    pprint.pprintln(
      (List(Foo("source"))).into[Vector[Bar]]
        .withFallback(Array(Foo("fallback1")))
        .withFallback(Set(Baz("fallback2")))
        .enableCollectionFallbackMerge(SourceAppendFallback)
        .transform
    )
    // expected output:
    // Vector(Bar(value = "source"), Bar(value = "fallback1"), Bar(value = "fallback2"))

    pprint.pprintln(
      (List(Foo("source"))).into[Vector[Bar]]
        .withFallback(Array(Foo("fallback1")))
        .withFallback(Set(Baz("fallback2")))
        .enableCollectionFallbackMerge(FallbackAppendSource)
        .transform
    )
    // expected output:
    // Vector(Bar(value = "fallback2"), Bar(value = "fallback1"), Bar(value = "source"))
    ```

## Implicit conversions

Implicit conversions are often considered a dangerous feature, which is why they are disabled by default.

However, sometimes one may need them, so it may be useful to be able to call them in macro. This can be enabled
with a flag `.enableImplicitConversions`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    import scala.language.implicitConversions

    implicit def convert(a: Int): String = a.toString

    pprint.pprintln(
      10.into[String].enableImplicitConversions.transform
    )
    // expected output:
    // "10"

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableImplicitConversions

      pprint.pprintln(
        10.transformInto[String]
      )
      // expected output:
      // "10"
    }
    ```

If the flag was enabled in the implicit config it can be disabled with `.disableImplicitConversions`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    import scala.language.implicitConversions

    implicit def convert(a: Int): String = a.toString

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableImplicitConversions

    10.into[String].disableImplicitConversions.transform
    // expected error:
    // Chimney can't derive transformation from scala.Int to java.lang.String
    //
    // java.lang.String
    //   derivation from int: scala.Int to java.lang.String is not supported in Chimney!
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

## Custom transformations

For virtually every 2 types that you want, you can define your own `Transformer` or `PartialTransformer` as `implicit`.

`Transformer`s are best suited for conversions that have to succeed because there is no value (of the transformed type)
for which they would not have a reasonable mapping:

!!! example

    From the moment you define an `implicit` `Transformer` it can be used any every other kind of transformation we
    described: 

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._

    implicit val int2string: Transformer[Int, String] = int => int.toString

    case class Foo(a: Int)
    case class Bar(a: String)

    pprint.pprintln(
      12.transformInto[Option[String]]
    )
    pprint.pprintln(
      Option(12).transformInto[Option[String]]
    )
    // expected output:
    // Some(value = "12")
    // Some(value = "12")

    pprint.pprintln(
      Foo(12).transformInto[Bar]
    )
    pprint.pprintln(
      List(Foo(10) -> 20).transformInto[Map[Bar, String]]
    )
    // expected output:
    // Bar(a = "12")
    // Map(Bar(a = "10") -> "20")
    ```

!!! warning

    Looking for an implicit `Transformer` and `PartialTransformer` is the first thing that Chimney does, to let you
    override any of the mechanics it uses.
    
    The only exception is a situation like:
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._

    case class Foo(a: Int)
    case class Bar(a: String)

    implicit val foo2bar: Transformer[Foo, Bar] = foo => Bar((foo.a * 2).toString)

    pprint.pprintln(
      Foo(10).into[Bar].withFieldConst(_.a, "value").transform
    )
    // expected output:
    // Bar(a = "value")
    ```
    
    If you pass field or coproduct overrides, they could not be applied if we used the implicit, so in such case Chimney
    assumed that the user wants to ignore the implicit.

!!! warning

    Make sure that you are:
    
     * **not** using `.transformInto`/`.transformIntoPartial`/`.patchUsing`
     * **nor** `.into.transform`/`.intoPartial.transform`/`.using.patch` **without overrides**
     * when transforming/patching **top-level object**

    as the code like:

    ```scala
    implicit val totalTransformer: Transformer[Foo, Bar] = (foo: Foo) => foo.transformInto[Bar]
    implicit val partialTransformer: PartialTransformer[Foo, Bar] = (foo: Foo) => foo.transformIntoPartial[Bar]
    implicit val patcher: Patcher[A, Patch] = (obj: A, patch: Patch) => obj.patchUsing(patch)
    ```

    will [create inifinite recursion in runtime and result in `StackOverflowError`](troubleshooting.md#recursive-calls-on-implicits).

    In such cases derive the code with `.derive`/.define` utilities:

    ```scala
    implicit val totalTransformer: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]
    implicit val partialTransformer: PartialTransformer[Foo, Bar] = PartialTransformer.derive[Foo, Bar]
    implicit val patcher: Patcher[A, Patch] = Patcher.derive[A, Patch]
    ```

    A manual definition of `Transfrormer`s/`PartialTransformer`s/`Patcher`s is necessary only when the derivation cannot happed
    for a particular type - and them using `.transformInto`/`.into.transform`/etc is usually not helpful.

    The only exception is when we would like to use the implicit we're currently deriving for some nested field as we're working with
    [recursive data structures (which can be safely handled with `.derive`/`.define`)](#recursive-data-types).

Total `Transformer`s can be utilized by `PartialTransformer`s as well - handling every input is a stronger guarantee
than handling only some of them, so we can always relax it:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._

    implicit val int2string: Transformer[Int, String] = int => int.toString

    case class Foo(a: Int)
    case class Bar(a: String)

    pprint.pprintln(
      Option(Foo(100))
        .transformIntoPartial[Bar]
        .asEither
    )
    pprint.pprintln(
      (None: Option[Foo])
        .transformIntoPartial[Bar]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = Bar(a = "100"))
    // Left(value = List(("", EmptyValue)))
    ```

Defining custom `PartialTransformer` might be a necessity when the type we want to transform has only some values which
can be safely converted, and some which have no reasonable mapping in the target type:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.{partial, PartialTransformer}
    import io.scalaland.chimney.dsl._

    implicit val string2int: PartialTransformer[String, Int] = PartialTransformer[String, Int] { string =>
      partial.Result.fromCatching(string.toInt) // catches exception which can be thrown by .toInt
    }

    case class Foo(a: Int)
    case class Bar(a: String)

    pprint.pprintln(
      "12".transformIntoPartial[Int].asEither.left.map(_.asErrorPathMessages)
    )
    pprint.pprintln(
      "bad"
        .transformIntoPartial[Int]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = 12)
    // Left(
    //   value = List(
    //     ("", ThrowableMessage(throwable = java.lang.NumberFormatException: For input string: "bad"))
    //   )
    // )

    pprint.pprintln(
      Bar("20").transformIntoPartial[Foo].asEither.left.map(_.asErrorPathMessages)
    )
    pprint.pprintln(
      Bar("wrong")
        .transformIntoPartial[Foo]
        .asEither
        .left
        .map(_.asErrorPathMessages)
    )
    // expected output:
    // Right(value = Foo(a = 20))
    // Left(
    //   value = List(
    //     ("a", ThrowableMessage(throwable = java.lang.NumberFormatException: For input string: "wrong"))
    //   )
    // )
    ```

!!! tip

    Partial Transformers are much more powerful than that! For other examples take a look at
    [Protocol Buffer integrations](cookbook.md#protocol-buffers-integration) and
    [Libraries with smart constructors](cookbook.md#libraries-with-smart-constructors).

### Custom transformers for `sealed`/`enum`s' subtypes

Providing transformations via `implicit`/`given` is possible for `sealed` hierarchies/`enum`s' subtypes as well -
when Chimney match subtypes by name, you can tell it how to convert them using implicits:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._

    sealed trait Foo
    object Foo {
      case object A extends Foo
      case class B(int: Int) extends Foo
    }

    sealed trait Bar
    object Bar {
      case class A(int: String) extends Bar
      case object B extends Bar
    }

    implicit val aToA: Transformer[Foo.A.type, Bar.A] = _ => Bar.A("10")
    implicit val bToB: Transformer[Foo.B, Bar.B.type] = _ => Bar.B

    pprint.pprintln(
      (Foo.A: Foo).transformInto[Bar]
    )
    pprint.pprintln(
      (Foo.B(42): Foo).transformInto[Bar]
    )
    // expected output:
    // A(int = "10")
    // B
    ```
    
However, usually it is easier to provide it via [an override](#handling-a-specific-sealed-subtype-with-a-computed-value)
instead.

!!! warning
    
    There also exist a special fallback rule for `sealed`/`enum` allowing to use a source's subtype to the whole target
    type:

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._

    sealed trait Foo
    object Foo {
      case object A extends Foo
      case class B(int: Int) extends Foo
    }

    sealed trait Bar
    object Bar {
      case class A(int: String) extends Bar
      case object C extends Bar
    }

    implicit val aToC: Transformer[Foo.A.type, Bar] = _ => Bar.A("a")
    implicit val bToD: Transformer[Foo.B, Bar] = b => if (b.int > 0) Bar.A(b.int.toString) else Bar.A("nope")

    pprint.pprintln(
      (Foo.A: Foo).transformInto[Bar]
    )
    pprint.pprintln(
      (Foo.B(42): Foo).transformInto[Bar]
    )
    pprint.pprintln(
      (Foo.B(-100): Foo).transformInto[Bar]
    )
    // expected output:
    // A(int = "a")
    // A(int = "42")
    // A(int = "nope")
    ```

    It has to be a fallback, to avoid cycles while resolving derivation.

### Resolving priority of implicit Total vs Partial Transformers

When you use Partial Transformers Chimney will try to:

   - summon the user-provided implicit - either `PartialTransformer` or `Transformer`
   - derive `PartialTransformer`

Under normal circumstances infallible transformation would be defined as `Transformer` and `PartialTransformer`s
would still be able to use it, so there is hardly ever the need for 2 instances for the same types.

However, you might write some generic `Transformer` and another generic `PartialTransformer` and for some type both of
them would exist. Since we have 2 types, we cannot use implicit priorities. Should Chimney assume that you might want
am infallible version if there are both? Or maybe you defined `Transformer` to do some unsafe behavior (for whatever
reason) and use `PartialTransformer` for safe implementation, and you prefer Partial.

The Chimney does not decide and in the presence of 2 implicits it will fail and ask you for preference:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.{partial, PartialTransformer, Transformer}
    import io.scalaland.chimney.dsl._

    implicit val stringToIntUnsafe: Transformer[String, Int] = _.toInt // throws!!!
    implicit val stringToIntSafe: PartialTransformer[String, Int] =
      PartialTransformer(str => partial.Result.fromCatching(str.toInt))

    "aa".intoPartial[Int].transform
    // expected error:
    // Chimney can't derive transformation from java.lang.String to scala.Int
    //
    //  scala.Int
    //   ambiguous implicits while resolving Chimney recursive transformation!
    //     PartialTransformer[java.lang.String, scala.Int]: stringToIntSafe
    //     Transformer[java.lang.String, scala.Int]: stringToIntUnsafe
    //   Please eliminate total/partial ambiguity from implicit scope or use enableImplicitConflictResolution/withFieldComputed/withFieldComputedPartial to decide which one should be used.
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```
    
    When we provide a way of resolving implicits, the error dissapears:
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.{partial, PartialTransformer, Transformer}
    import io.scalaland.chimney.dsl._

    implicit val stringToIntUnsafe: Transformer[String, Int] = _.toInt // throws!!!
    implicit val stringToIntSafe: PartialTransformer[String, Int] =
      PartialTransformer(str => partial.Result.fromCatching(str.toInt))

    locally {
      implicit val cfg = TransformerConfiguration.default.enableImplicitConflictResolution(PreferTotalTransformer)
      pprint.pprintln(
        "aa".transformIntoPartial[Int].asEither
      )
      // expected output:
      // Left(
      //   value = Errors(
      //     errors = NonEmptyErrorsChain(
      //       Error(
      //         message = ThrowableMessage(
      //           throwable = java.lang.NumberFormatException: For input string: "aa"
      //         ),
      //         path = Path(elements = List())
      //       )
      //     )
      //   )
      // )
    }
    locally {
      implicit val cfg = TransformerConfiguration.default.enableImplicitConflictResolution(PreferPartialTransformer)
      pprint.pprintln(
        "aa".transformIntoPartial[Int].asEither
      )
      // expected output:
      // Left(
      //   value = Errors(
      //     errors = NonEmptyErrorsChain(
      //       Error(
      //         message = ThrowableMessage(
      //           throwable = java.lang.NumberFormatException: For input string: "aa"
      //         ),
      //         path = Path(elements = List())
      //       )
      //     )
      //   )
      // )
    }
    ```

## Recursive transformation

When Chimney derives transformation it is a recursive process:

  - for each `class` into `case class`/POJO it will attempt recursion to find a mapping from the source field to
    the target constructor's argument/setter
  - for `sealed`/`enum`s it will attempt to convert each `case` pair recursively
  - for `AnyVal`s it will attempt to resolve mappings between inner values
  - for `Option`s and `Either`s and collections it will attempt to resolve mappings of the element types

etc.

The conditions for terminating the recursion are:

  - a failure to find a supported conversion (for every supported case at least one condition wasn't met, and users
    haven't provided their own via implicits)
  - the finding of user-provided `implicit` which handles the transformation between resolved types
  - proving that the source type is a subtype of the target type, so we can just upcast it.

### Recursive data types

Since we are talking about recursion then there is one troublesome issue - recursive data types.

!!! example

    ```scala
    case class Foo(a: Int, b: Option[Foo])
    case class Bar(a: Int, b: Option[Bar])

    val foo = Foo(10, Some(Foo(20, None)))
    // val bar = ??? // how to implement it?
    ``` 

We cannot derive an expression that would handle such data without any recursion (or other form of backtracking).

But we can use Chimney's semiautomatic derivation.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._

    case class Foo(a: Int, b: Option[Foo])
    case class Bar(a: Int, b: Option[Bar])

    implicit val foobar: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]

    val foo = Foo(10, Some(Foo(20, None)))
    val bar = foo.transformInto[Bar]
    pprint.pprintln(bar)
    // expected output:
    // Bar(a = 10, b = Some(value = Bar(a = 20, b = None)))
    ```

This is a smart method preventing cyclical dependencies during implicit resolution (`foobar = foobar`), but
will be able to call `foobar` within `foobar`'s definition in such a way that it won't cause issues.

If we need to customize it, we can use `.define.buildTransformer`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._

    case class Foo(a: Int, b: Option[Foo])
    case class Bar(a: Int, b: Option[Bar])

    implicit val foobar: Transformer[Foo, Bar] = Transformer
      .define[Foo, Bar]
      .withFieldComputed(_.a, foo => foo.a * 2)
      .buildTransformer

    val foo = Foo(10, Some(Foo(20, None)))
    val bar = foo.transformInto[Bar]
    pprint.pprintln(bar)
    // expected output:
    // Bar(a = 20, b = Some(value = Bar(a = 40, b = None)))
    ```

## Defining custom name matching predicate

Arguments taken by both `.enableCustomFieldNameComparison` and `.enableCustomSubtypeNameComparison` are values of type
`TransformedNamesComparison`. Out of the box, Chimney provides:

 - `TransformedNamesComparison.StrictEquality` - 2 names are considered equal only if they are identical `String`s.
   This is the default matching strategy for subtype names comparison
 - `TransformedNamesComparison.BeanAware` - 2 names are considered equal if they are identical `String`s OR if they are
   identical after you convert them from Java Bean naming convention: 
    - if a name starts with `is`/`get`/`set` prefix (e.g. `isField`, `getField`, `setField`) then
    - strip this name from the prefix (obtaining e.g. `Field`) and
    - lower case the first letter (obtaining e.g. `field`)
    
 - `TransformedNamesComparison.CaseInsensitiveEquality` - 2 names are considered equal if `equalsIgnoreCase` returns
  `true`

However, these 3 do not exhaust all possible comparisons and you might need to provide one yourself. 

!!! warning

    This is an advanced feature! Due to macros' limitations this feature requires several conditions to work.

The challenge is that the function you'd like to provide has to be called within macro, so it has to be defined in such
a way that the macro will be able to access it. Normally, there is no way to inject a custom login into existing macro,
but Chimney has a specific solution for this:

 - you need to define your `TransformedNamesComparison` as `object` - objects do not need constructor arguments, so
   they can be instantiated easily
 - your have to define this `object` as top-level definition or within another object - object defined within a `class`,
   a `trait` or locally, does need some logic for instantiation
 - you have to define your `object` in a module/subproject that is compiled _before_ the module where you need to use
   it, so that the bytecode would already be accessible on the classpath.

!!! example

    ```scala
    // file: your/organization/PermissiveNamesComparison.scala - part of custom naming comparison example
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    package your.organization

    import io.scalaland.chimney.dsl._

    // Allows matching: UPPERCASE, lowercase, kebab-case, underline_case,
    // PascalCase, camelCase and Java Beans conventions together
    //
    // Object is "case" for better toString output.
    case object PermissiveNamesComparison extends TransformedNamesComparison {

      private def normalize(name: String): String = {
        val name2 =
          if (name.startsWith("is")) name.drop(2)
          else if (name.startsWith("get")) name.drop(3)
          else if (name.startsWith("set")) name.drop(3)
          else name
        name2.replaceAll("[-_]", "")
      }

      def namesMatch(fromName: String, toName: String): Boolean =
        normalize(fromName).equalsIgnoreCase(normalize(toName))
    }
    ```

    If you define this `object` in module A, and you want to use it in module B, where B depends on A, macros would
    be able to use that value.

    ```scala
    // file: your/organization/PermissiveNamesComparison.test.scala - part of custom naming comparison example
    //> using dep org.scalameta::munit::1.0.0
    import io.scalaland.chimney.dsl._

    case class Foo(a_name: String, BName: String)
    case class Bar(`a-name`: String, getBName: String)

    class Test extends munit.FunSuite {
      test("should compile") {
        Foo("value1", "value2")
          .into[Bar]
          .enableCustomFieldNameComparison(your.organization.PermissiveNamesComparison)
          // this would be parsed as well
          // .enableCustomSubtypeNameComparison(your.organization.PermissiveNamesComparison)
          .transform
      }
    }
    ```

Since this feature relied on ClassLoaders and class path lookup it, testing it with REPL may not work.
