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
the transformation, most of the time it could do it for **every** value of the source type. In Chimney we called such
transformations Total (because they are virtually **total functions**). One way in which Chimney allows you to use such
transformation is through `Transformer[From, To]`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.Transformer

    class MyType(val a: Int)
    class MyOtherType(val b: String)    

    val transformer: Transformer[MyType, MyOtherType] = (src: MyType) => new MyOtherType(src.a.toString)
        
    transformer.transform(new MyType(10)) // new MyOtherType("10")
    
    import io.scalaland.chimney.dsl._
    
    // When the compiler can find an implicit Transformer...
    implicit val transformerAsImplicit: Transformer[MyType, MyOtherType] = transformer
    
    // ...we can use this extension method to call it
    (new MyType(10)).transformInto[MyOtherType] // new MyOtherType("10")
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.{PartialTransformer, partial}

    class MyType(val b: String)
    class MyOtherType(val a: Int)

    val transformer: PartialTransformer[MyType, MyOtherType] = PartialTransformer[MyType, MyOtherType] { (src: MyType) =>
      partial.Result.fromCatching(src.b.toInt)
        .prependErrorPath(partial.PathElement.Accessor("b"))
        .map { a =>
          new MyOtherType(a)
        }
    }
    
    transformer.transform(new MyType("10")).asEither
      .left.map(_.asErrorPathMessages) // Right(new MyOtherType(10))
    transformer.transform(new MyType("NaN")).asEither
      .left.map(_.asErrorPathMessages) // Left(Iterable("b" -> ThrowableMessage(NumberFormatException: For input string: NaN)))
    
    import io.scalaland.chimney.dsl._
    
    // When the compiler can find an implicit Transformer...
    implicit val transformerAsImplicit: PartialTransformer[MyType, MyOtherType] = transformer
    
    // ...we can use this extension method to call it
    (new MyType("10")).transformIntoPartial[MyOtherType].asEither
      .left.map(_.asErrorPathMessages) // Right(new MyOtherType(10))
    (new MyType("NaN")).transformIntoPartial[MyOtherType].asEither
      .left.map(_.asErrorPathMessages) // Left(Iterable("b" -> ThrowableMessage(NumberFormatException: For input string: NaN)))
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

## Upcasting and identity transformation

If you transform one type into itself or its supertype, it will be upcast without any change.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._

    trait A
    class B extends A
    val b = new B
   
    b.transformInto[A]  // == (b: A)
    b.into[A].transform // == (b: A)
    b.transformIntoPartial[A].asEither  // == Right(b: A)
    b.intoPartial[A].transform.asEither // == Right(b: A)
    ```

In particular, when the source type is (`=:=`) the target type, you will end up with an identity transformation.

!!! warning

    Checking if value can be upcast is the second thing Chimney attempts (right after
    [looking for an implicit](#custom-transformations)).
    
    This attempt is only skipped if we customised the transformation:
    
    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class A(val a: String)
    class B extends A("value")
    val b = new B
    
    b.into[A].withFieldConst(_.a, "copied").transform // new A("copied")
    ```
    
    since that customization couldn't be applied if we only upcasted the value. 

## Into a `case class` (or POJO)

Every type can have its `val`s read and used as data sources for the transformation.

And every class with a public primary constructor can be the target of the transformation.

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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._

    case class Source(a: Int, b: Double)
    case class Target(a: Int, b: Double)

    Source(42, 0.07).transformInto[Target]  // == Target(42, 0.07)
    Source(42, 0.07).into[Target].transform // == Target(42, 0.07)
    Source(42, 0.07).transformIntoPartial[Target].asEither  // == Right(Target(42, 0.07))
    Source(42, 0.07).intoPartial[Target].transform.asEither // == Right(Target(42, 0.07))
    ```

However, the original value might have fields absent in the target type and/or appearing in a different order:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._

    case class Source(a: Int, b: Double, c: String)
    case class Target(b: Double, a: Int)

    Source(42, 0.07, "value").transformInto[Target]  // == Target(42, 0.07)
    Source(42, 0.07, "value").into[Target].transform // == Target(42, 0.07)
    Source(42, 0.07, "value").transformIntoPartial[Target].asEither  // == Right(Target(42, 0.07))
    Source(42, 0.07, "value").intoPartial[Target].transform.asEither // == Right(Target(42, 0.07))
    ```

It doesn't even have to be a `case class`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(baz: Foo.Baz)
    object Foo { case class Baz(baz: String) }

    case class Bar(baz: Bar.Baz)
    object Bar { case class Baz(baz: String) }

    Foo(Foo.Baz("baz")).transformInto[Bar]  // Baz(Bar.Baz("baz"))
    Foo(Foo.Baz("baz")).into[Bar].transform // Baz(Bar.Baz("baz"))
    Foo(Foo.Baz("baz")).transformIntoPartial[Bar].asEither  // Right(Baz(Bar.Baz("baz")))
    Foo(Foo.Baz("baz")).intoPartial[Bar].transform.asEither // Right(Baz(Bar.Baz("baz")))
    ```

As we see, for infallible transformations there is very little difference in behavior between Total and Partial
Transformers. For "products" the difference shows up when transformation for any field/constructor fails. One such fallible
transformation, available only in partial transformers, is unwrapping `Option` fields.

!!! example

    Partial Transformers preserve the path (with nestings!) to the failed transformation

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(baz: Foo.Baz)
    object Foo { case class Baz(baz: Option[String]) }

    case class Bar(baz: Bar.Baz)
    object Bar { case class Baz(baz: String) }
    
    Foo(Foo.Baz(Some("baz"))).transformIntoPartial[Bar].asEither.left.map(_.asErrorPathMessages)  // Right(Baz(Bar.Baz("baz")))
    Foo(Foo.Baz(None)).transformIntoPartial[Bar].asEither.left.map(_.asErrorPathMessages) // Left(Iterable("baz.bar" -> EmptyValue))
    ```

Examples so far assumed, that each constructor's argument was paired with a field of the same name. So, let's show what
to do if that isn't the case.

### Reading from methods

If we want to read from `def fieldName: A` as if it was `val fieldName: A` - which could be unsafe as it might perform
side effects - you need to enable the `.enableMethodAccessors` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(_a: String, _b: Int) {
      def a: String = _a
      def b(): Int = _b
    }
    class Target(a: String, b: Int)
    
    (new Source("value", 512)).into[Target].enableMethodAccessors.transform
    // val source = new Source("value", 512)
    // new Target(source.a, source.b())
    (new Source("value", 512)).intoPartial[Target].enableMethodAccessors.transform
    // val source = new Source("value", 512)
    // partial.Result.fromValue(new Target(source.a, source.b()))
    
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
      implicit val cfg = TransformerConfiguration.default.enableMethodAccessors
      
      (new Source("value", 512)).transformInto[Target]
      // val source = new Source("value", 512)
      // new Target(source.a, source.b())
      (new Source("value", 512)).transformIntoPartial[Target]
      // val source = new Source("value", 512)
      // partial.Result.fromValue(new Target(source.a, source.b()))
    }
    ```

Flag `.enableMethodAccessors` will allow macros to consider methods that are:

  - nullary (take 0 value arguments)
  - have no type parameters
  - cannot be considered Bean getters

If the flag was enabled in the implicit config it can be disabled with `.disableMethodAccessors`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(_a: String, _b: Int) {
      def a: String = _a
      def b(): Int = _b
    }
    class Target(a: String, b: Int)
    
    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableMethodAccessors
    
    (new Source("value", 512)).into[Target].disableMethodAccessors.transform
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

### Reading from inherited values/methods

Out of the box, only values defined directly within the source type are considered. If we want to read from `val`s
inherited from a source value's supertype, you need to enable the `.enableInheritedAccessors` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    trait Parent {
      val a = "value"
    }
    case class Source(b: Int) extends Parent
    case class Target(a: String, b: Int)
    
    Source(10).into[Target].enableInheritedAccessors.transform // Target("value", 10)
    Source(10).intoPartial[Target].enableInheritedAccessors.transform.asEither // Right(Target("value", 10))
    
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
      implicit val cfg = TransformerConfiguration.default.enableInheritedAccessors
      
      Source(10).transformInto[Target] // Target("value", 10)
      Source(10).transformIntoPartial[Target].asEither // Right(Target("value", 10))
    }
    ```

!!! tip

    `.enableInheritedAccessors` can be combined with [`.enableMethodAccessors`](#reading-from-inherited-valuesmethods)
    and [`.enableBeanGetters`](#reading-from-bean-getters) to allow reading from inherited `def`s and Bean getters.

If the flag was enabled in the implicit config it can be disabled with `.enableInheritedAccessors`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    trait Parent {
      val a = "value"
    }
    case class Source(b: Int) extends Parent
    case class Target(a: String, b: Int)
    
    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableInheritedAccessors
    
    Source(10).into[Target].disableInheritedAccessors.transform
    // Chimney can't derive transformation from Source to Target
    // 
    // Target
    //   a: java.lang.String - no accessor named a in source type Source
    // 
    // There are methods in Source that might be used as accessors for `a` fields in Target. Consider using `.enableMethodAccessors`.
    // 
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

### Reading from Bean getters

If we want to read `def getFieldName(): A` as if it was `val fieldName: A` - which would allow reading from Java Beans
(or Plain Old Java Objects) - you need to enable a flag: 

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(a: String, b: Int) {
      def getA(): String = a
      def getB(): Int = b
    }
    class Target(a: String, b: Int)
    
    (new Source("value", 512)).into[Target].enableBeanGetters.transform
    // val source = new Source("value", 512)
    // new Target(source.getA(), source.getB())
    (new Source("value", 512)).intoPartial[Target].enableBeanGetters.transform
    // val source = new Source("value", 512)
    // partial.Result.fromValue(new Target(source.getA(), source.getB()))
    
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
      implicit val cfg = TransformerConfiguration.default.enableBeanGetters
      
      (new Source("value", 512)).transformInto[Target]
      // val source = new Source("value", 512)
      // new Target(source.getA(), source.getB())
      (new Source("value", 512)).transformIntoPartial[Target]
      // val source = new Source("value", 512)
      // partial.Result.fromValue(new Target(source.getA(), source.getB()))
    }
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(a: String, b: Int) {
      def getA(): String = a
      def getB(): Int = b
    }
    class Target(a: String, b: Int)
    
    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableBeanGetters
    
    (new Source("value", 512)).into[Target].disableBeanGetters.transform
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(val a: String, val b: Int)
    class Target() {
      private var a = ""
      def setA(a_: String): Unit = a = a_
      private var b = 0
      def setB(b_: Int): Unit = b = b_
    }
    
    (new Source("value", 512)).into[Target].enableBeanSetters.transform
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.setA(source.a)
    // target.setB(source.b)
    // target
    (new Source("value", 512)).intoPartial[Target].enableBeanSetters.transform
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.setA(source.a)
    // target.setB(source.b)
    // partial.Result.fromValue(target)
    
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
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
    ```

Flag `.enableBeanSetters` will allow macros to write to methods which are:

  - unary (take 1 value argument)
  - have no type parameters
  - have names starting with `set` - for comparison `set` will be dropped and the first remaining letter lowercased

_besides_ calling constructor (so you can pass values to _both_ the constructor and setters at once). Without the flag
macro will fail compilation to avoid creating potentially uninitialized objects.

!!! warning

   0.8.0 dropped the requirement that the setter needs to return `Unit`. It enables targeting mutable builders, which
   let you chain calls with fluent API, but are still mutating the state internally, making this chaining optional.  

If the flag was enabled in the implicit config it can be disabled with `.disableBeanSetters`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(val a: String, val b: Int)
    class Target() {
      private var a = ""
      def getA: String = a
      def setA(aa: String): Unit = a = aa
      private var b = 0
      def getB(): Int = b
      def setB(bb : Int): Unit = b = bb
    }
    
    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableBeanSetters
    
    (new Source("value", 512)).into[Target].disableBeanSetters.transform
    // Chimney can't derive transformation from Source to Target
    //
    // Target
    //   derivation from source: Source to Target is not supported in Chimney!
    // 
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

### Ignoring unmatched Bean setters

If the target class has any method that Chimney recognized as a setter, by default it will refuse to generate the code
unless we explicitly tell it what to do with these setters. If using them is not what we intended, we can also ignore
them:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Target() {
      private var a = ""
      def getA: String = a
      def setA(aa: String): Unit = a = aa
      private var b = 0
      def getB(): Int = b
      def setB(bb : Int): Unit = b = bb
    }
    
    ().into[Target].enableIgnoreUnmatchedBeanSetters.transform // new Target()
    ().intoPartial[Target].enableIgnoreUnmatchedBeanSetters.transform // partial.Result.fromValue(new Target())
    
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
      implicit val cfg = TransformerConfiguration.default.enableIgnoreUnmatchedBeanSetters

      ().transformInto[Target] // new Target()
      ().transformIntoPartial[Target] // partial.Result.fromValue(new Target())
    }
    ```

If the flag was enabled in the implicit config it can be disabled with `.disableIgnoreUnmatchedBeanSetters`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Target() {
      private var a = ""
      def getA: String = a
      def setA(aa: String): Unit = a = aa
      private var b = 0
      def getB(): Int = b
      def setB(bb : Int): Unit = b = bb
    }
    
    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableIgnoreUnmatchedBeanSetters
    
    ().into[Target].disableIgnoreUnmatchedBeanSetters.transform
    // Chimney can't derive transformation from Source to Target
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(val a: String)
    class Target() {
      private var a = ""
      def setA(a_: String): Unit = a = a_
      private var b = 0
      def setB(b_: Int): Unit = b = b_
    }
    
    (new Source("value")).into[Target].enableBeanSetters.enableIgnoreUnmatchedBeanSetters.transform
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.setA(source.a)
    // target
    (new Source("value")).intoPartial[Target].enableBeanSetters.enableIgnoreUnmatchedBeanSetters.transform
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.setA(source.a)
    // partial.Result.fromValue(target)
    
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
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

### Fallback to `Unit` as the constructor's argument

If a class' constructor takes `Unit` as a parameter it is always provided without any configuration.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Source()
    case class Target(value: Unit)
    
    Source().transformInto[Target] // Target(())
    Source().into[Target].transform // Target(())
    Source().transformIntoPartial[Target].asEither // Right(Target(()))
    Source().intoPartial[Target].transform.asEither // Right(Target(()))
    ```

### Allowing fallback to the constructor's default values

When calling the constructor manually, sometimes we want to not pass all arguments ourselves and let the default values
handle the remaining ones. If Chimney did it out of the box, it could lead to some subtle bugs - you might prefer a
compilation error reminding you to provide the value yourself - but if you know that it is safe you can enable fallback
to default values with the `.enableDefaultValues` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Source(a: String, b: Int)
    case class Target(a: String, b: Int = 0, c: Long = 0L)
    
    Source("value", 128).into[Target].enableDefaultValues.transform
    // val source = Source("value", 128)
    // Target(source.a, source.b /* c is filled by the default value */)
    Source("value", 128).intoPartial[Target].enableDefaultValues.transform
    // val source = Source("value", 128)
    // partial.Result.fromValue(Target(source.a, source.b /* c is filled by the default value */))
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Source(a: String, b: Int)
    case class Target(a: String, b: Int = 0, c: Long = 0L)
    
    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableDefaultValues
    
    (new Source("value", 512)).into[Target].disableDefaultValues.transform
    // Chimney can't derive transformation from Source to Target
    // 
    // Target
    //   c: scala.Long - no accessor named c in source type Source
    //  
    // Consult https://chimney.readthedocs.io for usage examples.
    ```


### Allowing fallback to `None` as the constructor's argument

Sometimes we transform value into a type that would use `Option`'s `None` to handle some default behavior and
`Some` as the user's overrides. This type might not have a default value (e.g. `value: Option[A] = None`) in its
constructor, but we would find it useful to fall back on `None` in such cases. It is not enabled out of the box, for
similar reasons to default values support, but we can enable it with the `.enableOptionDefaultsToNone` flag:   

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String, b: Option[String] = Some("a"))
    
    // without flags -> compilation error
    Foo("value").into[Bar].enableOptionDefaultsToNone.transform // Bar("value", None)
    Foo("value").intoPartial[Bar].enableOptionDefaultsToNone.transform.asOption // Some(Bar("value", None))
    
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
      implicit val cfg = TransformerConfiguration.default.enableOptionDefaultsToNone
      
      Foo("value").transformInto[Bar] // Bar("value", None)
      Foo("value").transformIntoPartial[Bar].asOption // Some(Bar("value", None))
    }
    ```

The `None` value is used as a fallback, meaning:

  - it has to be enabled with a flag
  - it will not be used if you provided value manually with one of the `.with*` methods - then the value provision
    always succeeds
  - it will not be used if a source field (`val`) or a method (enabled with one of the flags above) with a matching name
    could be found - if a source value type can be converted into a target argument/setter type then the value provision
    succeeds, but if Chimney fails to convert the value then the whole derivation fails rather than falls back to
    the `None` value
  - it will not be used if a default value is present and [the support for default values has been enabled](#allowing-the-constructors-default-values)
    (the fallback to `None` has a lower priority than the fallback to a default value) 

!!! example

    Behavior when both [`.enableDefaultValues`](#allowing-the-constructors-default-values) and `.enableOptionDefaultsToNone` are used:

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String, b: Option[String] = Some("a"))
    
    Foo("value").into[Bar].enableDefaultValues.enableOptionDefaultsToNone.transform // Bar("value", Some("a"))
    Foo("value").intoPartial[Bar].enableDefaultValues.enableOptionDefaultsToNone.transform.asOption // Some(Bar("value", Some("a")))
    
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
      implicit val cfg = TransformerConfiguration.default.enableOptionDefaultsToNone.enableDefaultValues
      
      Foo("value").transformInto[Bar] // Bar("value", Some("a"))
      Foo("value").transformIntoPartial[Bar].asOption // Some(Bar("value", Some("a")))
    }
    ```
    
    The original default value has a higher priority than `None`.

If the flag was enabled in the implicit config it can be disabled with `.disableOptionDefaultsToNone`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String, b: Option[String] = Some("a"))

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableOptionDefaultsToNone
    
    Foo("value").into[Bar].disableOptionDefaultsToNone.transform
    // Chimney can't derive transformation from Foo to Bar
    // 
    // Bar
    //   b: scala.Option[java.lang.String] - no accessor named b in source type Foo
    //  
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

### Wiring the constructor's parameter to its source field

In some cases, there is no source field available of the same name as the constructor's argument. However, another field
could be used in this role. Other times the source field of the matching name exists, but we want to explicitly override
it with another field. Since the usual cause of such cases is a _rename_, we can handle it using `.withFieldRenamed`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String, b: Int)
    case class Bar(a: String, c: Int)
    
    Foo("value", 1248).into[Bar].withFieldRenamed(_.b, _.c).transform // Bar("value", 1248)
    Foo("value", 1248).intoPartial[Bar].withFieldRenamed(_.b, _.c).transform.asEither // Right(Bar("value", 1248))
    ```

!!! tip

    The intuition is that we are pointing at a field in a source `case class` then a field in target `case class`, and
    Chimney will use the value from the former to provide it to the latter.
    
    However, Chimney is **not** limited to `case class`es and we can provide a value for **every** constructor's
    argument as long as it has a matching `val`, that we can use in `_.targetName` hint.

The requirements to use a rename are as follows:

  - you have to pass `_.fieldName` directly, it cannot be done with a reference to the function
  - the field rename can be _nested_, you can pass `_.foo.bar.baz` there
  - you can only use `val`/nullary method/Bean getter as a source field name
  - you have to have a `val`/nullary method/Bean getter with a name matching constructor's argument (or Bean setter if
    setters are enabled)
 
The last 2 conditions are always met when working with `case class`es with no `private val`s in constructor, and classes
with all arguments declared as public `val`s, and Java Beans where each setter has a corresponding getter defined.

!!! example

    Field renaming with Java Beans 

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters
    
    (new Foo()).into[Bar].withFieldRenamed(_.getB(), _.getC).transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA) 
    // bar.setC(foo.getB())
    // bar
    (new Foo()).intoPartial[Bar].withFieldRenamed(_.getB(), _.getC).transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA) 
    // bar.setC(foo.getB())
    // partial.Result.fromValue(bar)
    ```

We are also able to rename fields in nested structure:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String, b: Int)
    case class Bar(a: String, c: Int)
    
    case class NestedFoo(foo: Foo)
    case class NestedBar(bar: Bar)
    
    NestedFoo(Foo("value", 1248))
      .into[NestedBar]
      .withFieldRenamed(_.foo, _.bar)
      .withFieldRenamed(_.foo.b, _.bar.c)
      .transform // NestedBar(Bar("value", 1248))
    NestedFoo(Foo("value", 1248))
      .intoPartial[NestedBar]
      .withFieldRenamed(_.foo, _.bar)
      .withFieldRenamed(_.foo.b, _.bar.c)
      .transform.asEither // Right(NestedBar(Bar("value", 1248)))
    ```

### Wiring the constructor's parameter to a provided value

Another way of handling the missing source field - or overriding an existing one - is providing the value for 
the constructor's argument/setter yourself. The successful value can be provided using `.withFieldConst`:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)
    
    // providing missing value...
    Foo("value", 10).into[Bar].withFieldConst(_.c, 1000L).transform // Bar("value", 10, 1000L)
    Foo("value", 10).intoPartial[Bar].withFieldConst(_.c, 1000L).transform.asEither // Right(Bar("value", 10, 1000L))
    // ...and overriding existing value
    Foo("value", 10).into[Bar].withFieldConst(_.c, 1000L).withFieldConst(_.b, 20).transform // Bar("value", 20, 1000L)
    Foo("value", 10).intoPartial[Bar].withFieldConst(_.c, 1000L).withFieldConst(_.b, 20).transform.asEither // Right(Bar("value", 20, 1000L))
    ```

`.withFieldConst` can be used to provide/override only _successful_ values. What if we want to provide a failure, e.g.:

  - a `String` with an error message
  - an `Exception`
  - or a notion of the empty value?

These cases can be handled only with `PartialTransformer` using `.withFieldConstPartial`:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    
    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)
    
    // successful partial.Result constant
    Foo("value", 10).intoPartial[Bar].withFieldConstPartial(_.c, partial.Result.fromValue(100L)).transform
      .asEither.left.map(_.asErrorPathMessages) // Right(Bar("value", 10, 1000L))
    // a few different partial.Result failures constants
    Foo("value", 10).intoPartial[Bar].withFieldConstPartial(_.c, partial.Result.fromEmpty).transform
      .asEither.left.map(_.asErrorPathMessages) // Left(Iterable("c", partial.Error.EmptyMessage))
    Foo("value", 10).intoPartial[Bar].withFieldConstPartial(_.c, partial.Result.fromErrorThrowable(new NullPointerException)).transform
      .asEither.left.map(_.asErrorPathMessages) // Left(Iterable("c", partial.Error.ThrowableMessage(e: NullPointerException)))
    Foo("value", 10).intoPartial[Bar].withFieldConstPartial(_.c, partial.Result.fromErrorString("bad value")).transform
      .asEither.left.map(_.asErrorPathMessages) // Left(Iterable("c", partial.Error.StringMessage("bad value")))
    ``` 

As you can see, the transformed value will automatically preserve the field name for which a failure happened.

!!! tip

    The intuition is that we are pointing at a field in a `case class` and provide a value for it.
    
    However, Chimney is **not** limited to `case class`es and we can provide a value for **every** constructor's
    argument as long as it has a matching `val`, that we can use in `_.targetName` hint.

The requirements to use a value provision are as follows:

  - you have to pass `_.fieldName` directly, it cannot be done with a reference to the function
  - the field value provision can be _nested_, you can pass `_.foo.bar.baz` there
  - you have to have a `val`/nullary method/Bean getter with a name matching constructor's argument (or Bean setter if
    setters are enabled)

The last conditions is always met when working with `case class`es with no `private val`s in constructor, and classes
with all arguments declared as public `val`s, and Java Beans where each setter has a corresponding getter defined.

!!! example

    Value provision with Java Beans 

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
    
    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters
    
    (new Foo()).into[Bar].withFieldConst(_.getC, 100).transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA) 
    // bar.setC(100L)
    // bar
    (new Foo()).intoPartial[Bar].withFieldConstPartial(_.getC, partial.Result.fromEmpty).transform
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)

    case class NestedFoo(foo: Foo)
    case class NestedBar(bar: Bar)
    
    NestedFoo(Foo("value", 1248))
      .into[NestedBar]
      .withFieldRenamed(_.foo, _.bar)
      .withFieldConst(_.bar.c, 1000L)
      .transform // NestedBar(Bar("value", 1248, 1000L))
    NestedFoo(Foo("value", 1248))
      .intoPartial[NestedBar]
      .withFieldRenamed(_.foo, _.bar)
      .withFieldConst(_.bar.c, 1000L)
      .transform.asEither // Right(NestedBar(Bar("value", 1248, 1000L)))
    ```

### Wiring the constructor's parameter to the computed value

Yet another way of handling the missing source field - or overriding an existing one - is computing the value for 
the constructor's argument/setter out from a whole transformed value. The always-succeeding transformation can be provided
using `.withFieldComputed`:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)
    
    // providing missing value...
    Foo("value", 10).into[Bar].withFieldComputed(_.c, foo => foo.b.toLong * 2).transform // Bar("value", 10, 20L)
    Foo("value", 10).intoPartial[Bar].withFieldComputed(_.c, foo => foo.b.toLong * 2).transform.asEither // Right(Bar("value", 10, 20L))
    // ...and overriding existing value
    Foo("value", 10).into[Bar]
      .withFieldComputed(_.c, foo => foo.b.toLong * 2)
      .withFieldComputed(_.b, foo => foo.b * 4)
      .transform // Bar("value", 40, 20L)
    Foo("value", 10).intoPartial[Bar]
      .withFieldComputed(_.c, foo => foo.b.toLong * 2)
      .withFieldComputed(_.b, foo => foo.b * 4)
      .transform.asEither // Right(Bar("value", 40, 20L))
    ```

`.withFieldComputed` can be used to compute only _successful_ values. What if we want to provide a failure, e.g.:

  - a `String` with an error message
  - an `Exception`
  - or a notion of the empty value?

These cases can be handled only with `PartialTransformer` using `.withFieldComputedPartial`:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    
    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)

    // always successful partial.Result
    Foo("value", 10).intoPartial[Bar]
      .withFieldComputedPartial(_.c, foo => partial.Result.fromValue(foo.b.toLong * 2))
      .transform.asEither.left.map(_.asErrorPathMessages) // Right(Bar("value", 10, 20L))
    // always failing with a partial.Result.fromErrorString
    Foo("value", 10).intoPartial[Bar]
      .withFieldComputedPartial(_.c, foo => partial.Result.fromErrorString("bad value"))
      .transform.asEither.left.map(_.asErrorPathMessages)// Left(Iterable("c" -> StringMessage("bad value")))
    // failure depends on the input (whether .toLong throws or not)
    Foo("20", 10).intoPartial[Bar]
      .withFieldComputedPartial(_.c, foo => partial.Result.fromCatching(foo.a.toLong))
      .transform.asEither.left.map(_.asErrorPathMessages)// Right(Bar("20", 10, 20L))
    Foo("value", 10).intoPartial[Bar]
      .withFieldComputedPartial(_.c, foo => partial.Result.fromCatching(foo.a.toLong))
      .transform.asEither.left.map(_.asErrorPathMessages)// Left(Iterable("c" -> ThrowableMessage(NumberFormatException: For input string: "value")))
    ``` 

As you can see, the transformed value will automatically preserve the field name for which failure happened.

!!! tip

    The intuition is that we are pointing at a field in a `case class` and computing a value for it.
    
    However, Chimney is **not** limited to `case class`es and we can compute a value for **every** constructor's
    argument as long as it has a matching `val`, that we can use in `_.targetName` hint.

The requirements to use a value computation are as follows:

  - you have to pass `_.fieldName` directly, it cannot be done with a reference to the function
  - the field value computation can be _nested_, you can pass `_.foo.bar.baz` there
  - you have to have a `val`/nullary method/Bean getter with a name matching constructor's argument (or Bean setter if
    setters are enabled)

The last conditions is always met when working with `case class`es with no `private val`s in constructor, and classes
with all arguments declared as public `val`s, and Java Beans where each setter has a corresponding getter defined.

!!! example

    Value computation with Java Beans 

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters
    
    (new Foo()).into[Bar].withFieldComputed(_.getC, foo => foo.getB().toLong).transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA) 
    // bar.setC(100L)
    // bar
    (new Foo()).intoPartial[Bar].withFieldComputedPartial(_.getC, foo => partial.Result.fromCatching(foo.getA.toLong)).transform
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
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)

    case class NestedFoo(foo: Foo)
    case class NestedBar(bar: Bar)
    
    NestedFoo(Foo("value", 1248))
      .into[NestedBar]
      .withFieldRenamed(_.foo, _.bar)
      .withFieldComputed(_.bar.c, nestedfoo => nestedfoo.foo.b.toLong * 2)
      .transform // NestedBar(Bar("value", 1248, 2496L))
    NestedFoo(Foo("value", 1248))
      .intoPartial[NestedBar]
      .withFieldRenamed(_.foo, _.bar)
      .withFieldComputedPartial(_.bar.c, nestedfoo => partial.Result.fromValue(nestedfoo.foo.b.toLong * 2))
      .transform.asEither // Right(NestedBar(Bar("value", 1248, 2496L)))
    ```

## From/into a `Tuple`

Conversion from/to a tuple of any size is almost identical to conversion between other classes. The only difference
is that when either the source or target type is a tuple, automatic matching between the source field and the target
constructor's argument is made by position instead of name:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String, b: Int, c: Long)
    
    Foo("value", 42, 1024L).transformInto[(String, Int, Long)] // ("value", 42, 1024L) 
    ("value", 42, 1024L).transformInto[Foo] // Foo("value", 42, 1024L)
    Foo("value", 42, 1024L).transformIntoPartial[(String, Int, Long)] // Right(("value", 42, 1024L)) 
    ("value", 42, 1024L).transformIntoPartial[Foo] // Right(Foo("value", 42, 1024L))
    ```

!!! tip

    You can use all the flags, renames, value provisions, and computations that are available to case classes,
    Java Beans and so on.

## From/into an `AnyVal`

`AnyVal`s can be used both as data sources for derivation as well as the targets of the transformation.

If `AnyVal` is the source, Chimney would attempt to unwrap it, and if it's the target wrap it - we treat `AnyVal`s
as transparent, similarly to virtually every other Scala library.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: Int) extends AnyVal
    case class Bar(b: Int) extends AnyVal
    
    Foo(10).into[Bar].transform // Bar(10)
    Foo(10).transformInto[Bar] // Bar(10)
    Foo(10).transformIntoPartial[Bar].asEither // Right(Bar(10))
    Foo(10).intoPartial[Bar].transform.asEither // Right(Bar(10))
    ```

!!! tip

    This behavior is non-configurable in Chimney, similar to how it is non-configurable in every other library. If you
    decided to use a derivation then libraries will wrap and upwrap `AnyVal`s for you automatically.
    
    If you don't want this behavior you can prevent it (in every library, not only Chimney) by making the `val`
    `private` - to prevent unwrapping - and/or making the constructor `private` - to prevent wrapping. This way you'd
    have to provide support for your type for each library by yourself.
    
    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(private val a: Int) extends AnyVal // cannot be automatically unwrapped
    case class Bar private (b: String) extends AnyVal // cannot be automatically wrapped
    
    Foo(10).transformInto[Bar]
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

!!! warning

    If you use any value override (`withFieldConst`, `withFieldComputed`, etc.) getting value from/to `AnyVal`, it
    _will_ be treated as just a normal product type.

## Between `sealed`/`enum`s

When both the source type and the target type of the transformation are `sealed` (`trait`, `abstract class`), Chimney
will convert the source type's subtypes into the target type's subtypes. To make it work out of the box, every source
type's subtype needs to have a corresponding subtype with a matching name in the target type:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
    
    (Foo.Baz("value", 10): Foo).transformInto[Bar] // Bar.Baz(10)
    (Foo.Baz("value", 10): Foo).into[Bar].transform // Bar.Baz(10)
    (Foo.Buzz: Foo).transformInto[Bar] // Bar.Buzz
    (Foo.Buzz: Foo).into[Bar].transform // Bar.Buzz
    (Foo.Baz("value", 10): Foo).transformIntoPartial[Bar].asEither // Right(Bar.Baz(10))
    (Foo.Baz("value", 10): Foo).intoPartial[Bar].transform.asEither // Right(Bar.Baz(10))
    (Foo.Buzz: Foo).transformIntoPartial[Bar].asEither // Right(Bar.Buzz)
    (Foo.Buzz: Foo).intoPartial[Bar].transform.asEither // Right(Bar.Buzz)
    ```

!!! tip

    You can remember that each `sealed`/`enum` would have to implement an exhaustive pattern matching to handle a whole
    input, and subtypes are matched by their names. So you can have more subtypes in the target type than there are in
    the source type. What you cannot have is a missing match.

It works also with Scala 3's `enum`:

!!! example

    `sealed trait` into `enum`

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl.*
    
    sealed trait Foo
    object Foo:
      case class Baz(a: String, b: Int) extends Foo
      case object Buzz extends Foo
    enum Bar:
      case Baz(b: Int)
      case Fizz  
      case Buzz
    
    (Foo.Baz("value", 10): Foo).transformInto[Bar] // Bar.Baz(10)
    (Foo.Buzz: Foo).transformInto[Bar] // Bar.Buzz
    ```
    
!!! example

    `enum` into `sealed trait` 

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl.*
    
    enum Foo:
      case Baz(a: String, b: Int)
      case Buzz
    sealed trait Bar
    object Bar:
      case class Baz(b: Int) extends Bar
      case object Fizz extends Bar  
      case object Buzz extends Bar
    
    (Foo.Baz("value", 10): Foo).transformInto[Bar] // Bar.Baz(10)
    (Foo.Buzz: Foo).transformInto[Bar] // Bar.Buzz
    ```
    
!!! example

    `enum` into `enum` 

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl.*
    
    enum Foo:
      case Baz(a: String, b: Int)
      case Buzz
    enum Bar:
      case Baz(b: Int)
      case Fizz  
      case Buzz
    
    (Foo.Baz("value", 10): Foo).transformInto[Bar] // Bar.Baz(10)
    (Foo.Buzz: Foo).transformInto[Bar] // Bar.Buzz
    ```

### Non-flat ADTs

To enable seamless work with [Protocol Buffers](cookbook.md#protocol-buffers-integration), there is also a special
handling for non-flat ADTs, where each subtype of a `sealed`/`enum` is a single-value wrapper around a `case class`.
In such cases, Chimney is able to automatically wrap/unwrap these inner values as if they were `AnyVal`s
(even though they are not!):

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
    (protobuf.A(protobuf.Foo.A("value", 42)) : protobuf.Foo).transformInto[domain.Bar] // domain.Bar.A("value", 42)
    (protobuf.B(protobuf.Foo.B()) : protobuf.Foo).transformInto[domain.Bar] // domain.Bar.B
    // unflattening
    (domain.Bar.A("value", 42): domain.Bar).transformInto[protobuf.Foo] // protobuf.A(Foo.A("value", 42)
    (domain.Bar.B : domain.Bar).transformInto[protobuf.Foo] // protobuf.B(Foo.B())
    ```

### Java's `enum`s

Java's `enum` can also be converted this way to/from `sealed`/Scala 3's `enum`/another Java's `enum`:

!!! example

    Java's `enum` to/from `sealed`

    ```java
    // in Java
    enum ColorJ {
      Red, Green, Blue;
    }
    ```

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    sealed trait ColorS
    object ColorS {
      case object Red extends ColorS
      case object Green extends ColorS
      case object Blue extends ColorS
    }
    
    ColorJ.Red.transformInto[ColorS] // ColorS.Red
    ColorJ.Green.transformInto[ColorS] // ColorS.Green
    ColorJ.Blue.transformInto[ColorS] // ColorS.Blue
    (ColorS.Red: ColorS).transformInto[ColorS] // ColorJ.Red
    (ColorS.Green: ColorS).transformInto[ColorS] // ColorJ.Green
    (ColorS.Blue: ColorS).transformInto[ColorS] // ColorJ.Blue
    ```

!!! example

    Java's `enum` to/from Scala's `enum`

    ```java
    // in Java
    enum ColorJ {
      Red, Green, Blue;
    }
    ```

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    enum ColorE:
      case Red, Green, Blue
    
    ColorJ.Red.transformInto[ColorE] // ColorE.Red
    ColorJ.Green.transformInto[ColorE] // ColorE.Green
    ColorJ.Blue.transformInto[ColorE] // ColorE.Blue
    (ColorE.Red: ColorS).transformInto[ColorS] // ColorJ.Red
    (ColorE.Green: ColorS).transformInto[ColorS] // ColorJ.Green
    (ColorE.Blue: ColorS).transformInto[ColorS] // ColorJ.Blue
    ```

### Handling a specific `sealed` subtype with a computed value

Sometimes we are missing a corresponding subtype of the target type. Or we might want to override it with our
computation. This can be done using `.withCoproductInstance`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
    
    (Bar.Baz("value"): Bar).into[Foo].withCoproductInstance[Bar.Fizz.type] {
      fizz => Foo.Baz(fizz.toString)
    }.transform // Foo.Baz("value")
    (Bar.Fizz: Bar).into[Foo].withCoproductInstance[Bar.Fizz.type] {
      fizz => Foo.Baz(fizz.toString)
    }.transform // Foo.Baz("Fizz")
    (Bar.Buzz: Bar).into[Foo].withCoproductInstance[Bar.Fizz.type] {
      fizz => Foo.Baz(fizz.toString)
    }.transform // Foo.Buzz
    ```

If the computation needs to allow failure, there is `.withCoproductInstancePartial`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
    
    (Bar.Baz("value"): Bar).intoPartial[Foo].withCoproductInstancePartial[Bar.Fizz.type] {
      fizz => partial.Result.fromEmpty
    }.transform.asEither // Right(Foo.Baz("value"))
    (Bar.Fizz: Bar).intoPartial[Foo].withCoproductInstancePartial[Bar.Fizz.type] {
      fizz => partial.Result.fromEmpty
    }.transform.asEither // Left(...)
    (Bar.Buzz: Bar).intoPartial[Foo].withCoproductInstancePartial[Bar.Fizz.type] {
      fizz => partial.Result.fromEmpty
    }.transform.asEither // Right(Foo.Buzz)
    ```

!!! warning

    Due to limitations of Scala 2, when you want to use `.withCoproductInstance` or `.withCoproductInstancePartial` with
    Java's `enum`s, the enum instance's exact type will always be upcasted/lost, turning the handler into "catch-all":

    ```java
    // in Java
    enum ColorJ {
      Red, Blue, Greed, Black;
    }
    ```

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    sealed trait ColorS
    object ColorS {
      case object Red extends ColorS
      case object Green extends ColorS
      case object Blue extends ColorS
    }
    
    def blackIsRed(black: ColorJ.Black.type): ColorS = ColorS.Red
    
    ColorJ.Red.into[ColorS].withCoproductInstance[ColorJ.Black.type](blackIsRed(_)).transform // ColorS.Red
    ColorJ.Green.into[ColorS].withCoproductInstance[ColorJ.Black.type](blackIsRed(_)).transform // ColorS.Red
    ColorJ.Blue.into[ColorS].withCoproductInstance[ColorJ.Black.type](blackIsRed(_)).transform // ColorS.Red
    ColorJ.Black.into[ColorS].withCoproductInstance[ColorJ.Black.type](blackIsRed(_)).transform // ColorS.Red
    ```
    
    There is nothing we can do about the type, however, we can analyze the code and, if it preserves the exact Java enum
    we can use a sort of a type refinement to remember the selected instance:
    
    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    sealed trait ColorS
    object ColorS {
      case object Red extends ColorS
      case object Green extends ColorS
      case object Blue extends ColorS
    }
    
    def blackIsRed(black: ColorJ.Black.type): ColorS = ColorS.Red
    
    ColorJ.Red.into[ColorS].withCoproductInstance { (black: ColorJ.Black.type) =>
      blackIsRed(black)
    }.transform // ColorS.Red
    ColorJ.Green.into[ColorS].withCoproductInstance { (black: ColorJ.Black.type) =>
      blackIsRed(black)
    }.transform // ColorS.Green
    ColorJ.Blue.into[ColorS].withCoproductInstance { (black: ColorJ.Black.type) =>
      blackIsRed(black)
    }.transform // ColorS.Blue
    ColorJ.Black.into[ColorS].withCoproductInstance { (black: ColorJ.Black.type) =>
      blackIsRed(black)
    }.transform // ColorS.Black
    ```
    
    This issue doesn't occur on Scala 3, which infers types correctly:
    
    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl.*
    
    enum ColorS:
      case Red, Green, Blue
    
    def blackIsRed(black: ColorJ.Black.type): ColorS = ColorS.Red
    
    ColorJ.Red.into[ColorS].withCoproductInstance(blackIsRed).transform // ColorS.Red
    ColorJ.Green.into[ColorS].withCoproductInstance(blackIsRed).transform // ColorS.Green
    ColorJ.Blue.into[ColorS].withCoproductInstance(blackIsRed).transform // ColorS.Blue
    ColorJ.Black.into[ColorS].withCoproductInstance(blackIsRed).transform // ColorS.Black
    ```

## From/into an `Option`

`Option` type has special support during the derivation of a transformation.

The transformation from one `Option` into another is obviously always supported:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String)
    
    Option(Foo("value")).transformInto[Option[Bar]] // Some(Bar("value"))
    (None : Option[Foo]).transformInto[Option[Bar]] // None
    Option(Foo("value")).into[Option[Bar]].transform // Some(Bar("value"))
    (None : Option[Foo]).into[Option[Bar]].transform // None
    Option(Foo("value")).transformIntoPartial[Option[Bar]].asEither // Right(Some(Bar("value")))
    (None : Option[Foo]).transformIntoPartial[Option[Bar]].asEither // Right(None)
    Option(Foo("value")).intoPartial[Option[Bar]].transform.asEither // Right(Some(Bar("value")))
    (None : Option[Foo]).intoPartial[Option[Bar]].transform.asEither // Right(None)
    ```

Additionally, an automatic wrapping with `Option` is also considered safe and always available:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String)
    
    Foo("value").transformInto[Option[Bar]] // Some(Bar("value"))
    Foo("value").into[Option[Bar]].transform // Some(Bar("value"))
    Foo("value").transformIntoPartial[Option[Bar]].asEither // Right(Some(Bar("value")))
    Foo("value").intoPartial[Option[Bar]].transform.asEither // Right(Some(Bar("value")))
    ```

However, unwrapping of an `Option` is impossible without handling `None` case, that's why Chimney handles it
automatically only with `PartialTransformer`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String)
    
    Option(Foo("value")).transformIntoPartial[Bar].asEither.left.map(_.asErrorPathMessages) // Right(Bar("value"))
    (None : Option[Foo]).transformIntoPartial[Bar].asEither.left.map(_.asErrorPathMessages) // Left(Iterable("" -> EmptyValue))
    Option(Foo("value")).intoPartial[Bar].transform.asEither.left.map(_.asErrorPathMessages) // Right(Bar("value"))
    (None : Option[Foo]).intoPartial[Bar].transform.asEither.left.map(_.asErrorPathMessages) // Left(Iterable("" -> EmptyValue))
    ```

!!! tip

    Out of the box, Chimney supports only Scala's build-in `Option`s.
    
    If you need to integrate with Java's `Optional`, please, read about
    [Java's collections integration](cookbook.md#java-collections-integration).
    
    If you need to provide support for your optional types, please, read about
    [custom optional types](cookbook.md#custom-optional-types).
    
## Between `Either`s

A transformation from one `Either` to another is supported as long as both left and right types can also be converted:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String)
    
    (Left(Foo("value")) : Either[Foo, Bar]).transformInto[Either[Bar, Foo]] // Left(Bar("value"))
    (Right(Bar("value")) : Either[Foo, Bar]).transformInto[Either[Bar, Foo]] // Right(Foo("value"))
    (Left(Foo("value")) : Either[Foo, Bar]).transformIntoPartial[Either[Bar, Foo]].asOption // Some(Left(Bar("value")))
    (Right(Bar("value")) : Either[Foo, Bar]).transformIntoPartial[Either[Bar, Foo]].asOption // Some(Right(Foo("value")))
    ```

A transformation from `Left` and `Right` into `Either` requires existence of only the transformation from the type we
know for sure is inside to their corresponding type in target `Either`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
     
    case class Foo(a: String, b: Int)
    case class Bar(a: String)
    case class Baz(a: String, b: Int, c: Long)

    // Foo -> Bar - can be derived
    // Foo -> Baz - cannot be derived without providing c
    (Left(Foo("value", 10))).transformInto[Either[Bar, Baz]] // Left(Bar("value"))
    (Right(Foo("value", 10))).transformInto[Either[Baz, Bar]] // Right(Bar("value"))
    ```

## Between Scala's collections/`Array`s

Every `Array`/every collection extending `scala.collection.Iterable` can be used as a source value for a collection's
transformation.

Every `Array`/every collection provided with `scala.collection.compat.Factory` can be used as a target type for a
collection's transformation.

The requirement for a collection's transformation is that both source's and target's conditions are met and that
the types stored within these collections can also be converted. 

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    import scala.collection.immutable.ListMap
     
    case class Foo(a: String)
    case class Bar(a: Option[String])
    
    List(Foo("value")).transformInto[Vector[Bar]] // Vector(Bar(Some("value")))
    Map(Foo("key") -> Foo("value")).transformInto[Array[(Bar, Bar)]] // Array(Bar(Some("key")) -> Bar(Some("value")))
    Vector(Foo("key") -> Foo("value")).transformInto[ListMap[Bar, Bar]] // ListMap(Bar(Some("key")) -> Bar(Some("value")))
    ```

With `PartialTransformer`s ware able to handle fallible conversions, tracing at which key/index the failure occurred:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: Option[String])

    List(Bar(Some("value")), Bar(None)).transformIntoPartial[Vector[Foo]]
      .asEither.left.map(_.asErrorPathMessages) // Left(Iterable("(1).a" -> EmptyValue))
    Map(Bar(Some("value")) -> Bar(None), Bar(None) -> Bar(Some("value"))).transformIntoPartial[Vector[(Foo, Foo)]]
      .asEither.left.map(_.asErrorPathMessages) // Left(Iterable("(Bar(Some(value))).a" -> EmptyValue, "keys(Bar(None))" -> EmptyValue))
    ```

!!! tip

    Out of the box, Chimney supports only Scala's build-in collections, which are extending `Iterable` and have
    `scala.collection.compat.Factory` provided as an implicit.
    
    If you need to integrate with Java's collections, please, read about
    [Java's collections integration](cookbook.md#java-collections-integration).
    
    If you need to provide support for your collection types, you have to write your own implicit methods. 

## Parametric types/generics

The Transformation from/to the parametric type can always be derived, when Chimney know how to transform each value
defined with a type parameter.

The most obvious case is having all type parameters applied to non-abstract types:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo[A](value: A)
    case class Bar[A](value: A)
    
    case class Baz[A](value: A)
    
    Foo(Baz("value")).transformInto[Bar[Baz[String]]] // Bar(Baz("value"))
    ```

or having type parameter being not used at all:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._

    type AbstractType1
    type AbstractType2

    case class Foo[A](value: String)
    case class Bar[A](value: String)
    
    Foo[AbstractType1]("value").transformInto[Bar[AbstractType2]] // Bar[AbstractType2]("value")
    ```

If the type is `abstract` and used as a value, but contains enough information that one of existing rules
knows how to apply it, the transformation can still be derived:

!!! example
 
    If Chimney knows that type can be safely upcasted, the upcasting is available to it:

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo[A](value: A)
    case class Bar[A](value: A)
    
    def upcastingExample[T, S >: T](foo: Foo[T]): Bar[S] =
      foo.transformInto[Bar[S]]
    
    upcastingExample[Int, AnyVal](Foo(10))
    ```
    
    If we don't know the exact type but we know enough to read the relevant fields, we can also do it:

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    trait Baz[A] { val value: A }
    case class Foo[A](value: A) extends Baz[A]
    case class Bar[A](value: A)
    
    def subtypeExample[T <: Baz[String]](foo: Foo[T]): Bar[Bar[String]] =
      foo.transformInto[Bar[Bar[String]]]
    
    subtypeExample(Foo(Foo("value")))
    ```
    
    On Scala 2, we are even able to use refined types (Scala 3, changed a bit how they works):

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo[A](value: A)
    case class Bar[A](value: A)
    
    def refinedExample[T <: { val value: String }](foo: Foo[T]): Bar[Bar[String]] =
      foo.into[Bar[Bar[String]]].enableMacrosLogging.transform
    
    refinedExample[Foo[String]](Foo(Foo("value")))
    ```

Finally, you can always provide a custom `Transformer` from/to a type containing a type parameter, as an `implicit`:

!!! example
 
    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.Transformer
    
    case class Foo[A](value: A)
    case class Bar[A](value: A)

    def conversion[T, S](foo: Foo[T])(implicit transformer: Transformer[T, S]): Bar[S] =
      foo.transformInto[Bar[S]]
    ```

!!! tip

    For more information about defining custom `Transformer`s and `PartialTransformer`s, you read the section below.
    
    If you need to fetch and pass around implicit transformers (both total and partial), read
    the [Automatic, semiautomatic and inlined derication](cookbook.md#automatic-semiautomatic-and-inlined-derivation)
    cookbook.

## Types with manually provided constructors

If your type cannot be constructed with a public primary constructor, is not a Scala collection, Option, `AnyVal`, etc
BUT you do know a way of constructing this type using a method - or handwritten lambda - you can point to that method.
Then Chimney will try to match the source type's getters against the method's parameters by their names:  

!!! example
 
    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(value: Int)
    case class Bar private (value: String)
    object Bar {
      def make(value: Int): Bar = Bar(value.toString) 
    }

    Foo(10).into[Bar].withConstructor(Bar.make).transform // Bar("10")
    
    Foo(10).into[Bar].withConstructor { (value: Int) =>
      Bar.make(value * 100)
    }.transform // Bar("1000")
    ```

!!! warning

    The current implementation has a limit of 22 arguments even on Scala 3 (it doesn't use `scala.FunctionXXL`).
    
    It also requires that you either pass a method (which will be Eta-expanded) or a lambda with _all_ parameters names
    (to allow matching parameters by name). It allows the method to have multiple parameters list and lambda to be
    defined as curried (`(a: A, b: B) => (c: C) => { ... }`). 

If your type only has smart a constructor which e.g. validates the input and might fail, you can provide a that smart
constructor for `PartialTransformer`:

!!! example
 
    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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

    Foo("10")
      .intoPartial[Bar]
      .withConstructorPartial(smartConstructor)
      .transform.asEither // Right(Bar(10))
    
    Foo("10")
      .intoPartial[Bar]
      .withConstructorPartial { (value: String) =>
        partial.Result.fromEitherString(Bar.parse(value))
      }.transform.asEither // Right(Bar(1000))
    ```

You can use this to automatically match the source's getters e.g. against Scala 3's `opaque type`'s constructor's
arguments - these types would almost always have methods which the user could recognize as constructor's but which might
be difficult to be automatically recognized as such: 

!!! example
 
    Due to nature of `opaque type`s to work this example needs to have opaque types defined in a different `.scala`
    file than where they are being used:

    ```scala
    package models
    
    case class Foo(value: String)
    
    opaque type Bar = Int
    extension (bar: Bar)
      def value: Int = bar
    object Bar {
      def parse(value: String): Either[String, Bar] =
        scala.util.Try(value.toInt).toEither.left.map(_.getMessage)
    }
    ```

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    package example
    
    import io.scalaland.chimney.dsl.*
    import io.scalaland.chimney.{partial, PartialTransformer}
    import models.{Foo, Bar}
    
    given PartialTransformer[Foo, Bar] = PartialTransformer.define[Foo, Bar]
      .withConstructorPartial { (value: String) =>
        partial.Result.fromEitherString(Bar.parse(value))
      }.buildTransformer
    
    @main def example: Unit =
      println(Foo("10").transformIntoPartial[Bar].asEither)
    ```

!!! tip 

    `opaque type`s usually have only one constructor argument and usually it is easier to not transform them that way,
    but rather call their constructor directly. If `opaque type`s are nested in the transformed structure, it might be
    easier to define [a custom transformer](#custom-transformations), perhaps by using a dedicated new type/refined type
    library and [providing an integration for all of its types](cookbook.md#libraries-with-smart-constructors).  

## Custom transformations

For virtually every 2 types that you want, you can define your own `Transformer` or `PartialTransformer` as `implicit`.

`Transformer`s are best suited for conversions that have to succeed because there is no value (of the transformed type)
for which they would not have a reasonable mapping:

!!! example

    From the moment you define an `implicit` `Transformer` it can be used any every other kind of transformation we
    described: 

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._
    
    implicit val int2string: Transformer[Int, String] = int => int.toString
    
    case class Foo(a: Int)
    case class Bar(a: String)

    12.transformInto[Option[String]] // Some("12")
    Option(12).transformInto[Option[String]] // Some("12")
    Foo(12).transformInto[Bar] // Bar("12")
    List(Foo(10) -> 20).transformInto[Map[Bar, String]] // Map(Bar("10") -> "20")
    ```

!!! warning

    Looking for an implicit `Transformer` and `PartialTransformer` is the first thing that Chimney does, to let you
    override any of the mechanics it uses.
    
    The only exception is a situation like:
    
    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._

    case class Foo(a: Int)
    case class Bar(a: String)    

    implicit val foo2bar: Transformer[Foo, Bar] = foo => Bar((foo.a * 2).toString) 
    
    Foo(10).into[Bar].withFieldConst(_.a, "value").transform // Bar("value")
    ```
    
    If you pass field or coproduct overrides, they could not be applied if we used the implicit, so in such case Chimney
    assumed that the user wants to ignore the implicit. 

Total `Transformer`s can be utilized by `PartialTransformer`s as well - handling every input is a stronger guarantee
than handling only some of them, so we can always relax it:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}    
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._
    
    implicit val int2string: Transformer[Int, String] = int => int.toString
    
    case class Foo(a: Int)
    case class Bar(a: String)
    
    Option(Foo(100)).transformIntoPartial[Bar].asEither // Right(Bar("100"))
    (None : Option[Foo]).transformIntoPartial[Bar].asEither.left.map(_.asErrorPathMessages) // Left(Iterable("" -> EmptyValue)) 
    ```

Defining custom `PartialTransformer` might be a necessity when the type we want to transform has only some values which
can be safely converted, and some which have no reasonable mapping in the target type:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}   
    import io.scalaland.chimney.{PartialTransformer, partial}
    import io.scalaland.chimney.dsl._
    
    implicit val string2int: PartialTransformer[String, Int] = PartialTransformer[String, Int] { string =>
      partial.Result.fromCatching(string.toInt) // catches exception which can be thrown by .toInt
    }
    
    case class Foo(a: Int)
    case class Bar(a: String)
    
    "12".transformIntoPartial[Int].asEither
      .left.map(_.asErrorPathMessages) // Right(12)
    "bad".transformIntoPartial[Int].asEither
      .left.map(_.asErrorPathMessages) // Left(Iterable("" -> ThrowableMessage(NumberFormatException: For input string: "bad")))
    Bar("20").transformIntoPartial[Foo] .asEither
      .left.map(_.asErrorPathMessages) // Right(Foo(20))
    Bar("wrong").transformIntoPartial[Foo] .asEither
      .left.map(_.asErrorPathMessages) // Left("a" -> ThrowableMessage(NumberFormatException: For input string: "bad"))
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
    
    (Foo.A: Foo).transformInto[Bar] // Bar.A(10)
    (Foo.B(42): Foo).transformInto[Bar] // Bar.B
    ```
    
However, usually it is easier to provide it via [an override](#handling-a-specific-sealed-subtype-with-a-computed-value)
instead.

!!! warning
    
    There also exist a special fallback rule for `sealed`/`enum` allowing to use a source's subtype to the whole target
    type:

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
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
    implicit val bToD: Transformer[Foo.B, Bar] = b =>
      if (b.int > 0) Bar.A(b.int.toString) else Bar.A("nope")
    
    (Foo.A: Foo).transformInto[Bar] // Bar.A(10)
    (Foo.B(42): Foo).transformInto[Bar] // Bar.B
    (Foo.B(-100): Foo).transformInto[Bar] // Bar.B
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
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}   
    import io.scalaland.chimney.{Transformer, PartialTransformer, partial}
    import io.scalaland.chimney.dsl._
    
    implicit val stringToIntUnsafe: Transformer[String, Int] = _.toInt // throws!!!
    implicit val stringToIntSafe: PartialTransformer[String, Int] =
      PartialTransformer(str => partial.Result.fromCatching(str.toInt))
      
    "aa".intoPartial[Int].transform
    // Ambiguous implicits while resolving Chimney recursive transformation:
    // 
    // PartialTransformer[java.lang.String, scala.Int]: stringToIntSafe
    // Transformer[java.lang.String, scala.Int]: stringToIntUnsafe
    // 
    // Please eliminate ambiguity from implicit scope or use enableImplicitConflictResolution/withFieldComputed/withFieldComputedPartial to decide which one should be used
    "aa".intoPartial[Int].enableImplicitConflictResolution(PreferTotalTransformer).transform // throws NumberFormatException: For input string: "aa"
    "aa".intoPartial[Int].enableImplicitConflictResolution(PreferPartialTransformer).transform.asOption // None
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
    val bar = ... // ???
    ``` 

We cannot derive an expression that would handle such data without any recursion (or other form of backtracking).

But we can use Chimney's semiautomatic derivation.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}   
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: Int, b: Option[Foo])
    case class Bar(a: Int, b: Option[Bar])
    
    implicit val foobar: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]
    
    val foo = Foo(10, Some(Foo(20, None)))
    val bar = foo.transformInto[Bar]
    ```

This is a smart method preventing cyclical dependencies during implicit resolution (`foobar = foobar`), but
will be able to call `foobar` within `foobar`'s definition in such a way that it won't cause issues.

If we need to customize it, we can use `.define.buildTransformer`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}   
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: Int, b: Option[Foo])
    case class Bar(a: Int, b: Option[Bar])
    
    implicit val foobar: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
      .withFieldComputed(_.a, foo => foo.a * 2)
      .buildTransformer
    
    val foo = Foo(10, Some(Foo(20, None)))
    val bar = foo.transformInto[Bar]
    ```
