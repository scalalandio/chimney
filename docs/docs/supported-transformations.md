# Supported Transformations

Chimney goes an extra mile to provide you will many reasonable transformations out of the box. Only if it isn't obvious
from the types, you need to provide it with a hint, but nothing more.

!!! note

    For your convenience all examples will be presented as snippets runnable from
    [Scala CLI](https://scala-cli.virtuslab.org/). You are able to copy the content, paste it into new `.scala` file,
    and compile by running a command in the file's folder:

    ```bash
    # scala_version - e.g. {{ scala.2_12 }}, {{ scala.2_13 }} or {{ scala.3 }}
    # platform      - e.g. jvm, scala-js or scala-native
    scala-cli compile --scala $scala_version --platform $platform .
    ```

## Upcasting and identity transformation

If you transform one type into itself or into its supertype, it will be upcasted without any change.

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._

    trait A
    class B
    val b = new B
   
    b.transformInto[A]  // == (b: A)
    b.into.transform[A] // == (b: A)
    b.transformIntoPartial[A].asEither  // == Right(b: A)
    b.intoPartial[A].transform.asEither // == Right(b: A)
    ```

In particular, when the source type `=:=` the target type, you will end up with an identity transformation.

## Into a `case class` (or POJO)

Every type can have its `val`s read as used as data sources for transformation.

And every class with a public primary constructor can be the target of the transformation.

To make it work out of the box, every argument of a constructor needs to be paired with a matching field (`val`) in the
transformed value.

The obvious example are `case class`es with the same fields:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._

    case class Source(a: Int, b: Double)
    case class Target(a: Int, b: Double)

    Source(42, 0.07).trasnformInto[Target]  // == Target(42, 0.07)
    Source(42, 0.07).into[Target].transform // == Target(42, 0.07)
    Source(42, 0.07).trasnformIntoPartial[Target].asEither  // == Right(Target(42, 0.07))
    Source(42, 0.07).intoPartial[Target].transform.asEither // == Right(Target(42, 0.07))
    ```

However, original value might have fields absent in target type, and appearing in different order:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._

    case class Source(a: Int, b: Double, c: String)
    case class Target(b: Double, a: Int)

    Source(42, 0.07).trasnformInto[Target]  // == Target(42, 0.07)
    Source(42, 0.07).into[Target].transform // == Target(42, 0.07)
    Source(42, 0.07).trasnformIntoPartial[Target].asEither  // == Right(Target(42, 0.07))
    Source(42, 0.07).intoPartial[Target].transform.asEither // == Right(Target(42, 0.07))
    ```

It doesn't even have to be a `case class`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source {
      val a: String = "a"
      val b: Int = 1024
    }
    class Target(a: String, b: Int)

    (new Source).trasnformInto[Target]
    // like:
    // val source = new Source
    // new Target(source.a, source.b)
    ```

nor have the same types of fields as long as transformation for each pair field-constructor's argument can be resolved
recursively:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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

As we see, for infallible transformations there is very little difference in behavior between total and partial
transformers. For "products" the difference shows when transformation for any field/constructor fails. One such fallible
transformation, available only in partial transformers, is unwrapping `Option` fields.

!!! example

    Partial Transformers preserve path (with nestings!) to the failed transformation

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(baz: Foo.Baz)
    object Foo { case class Baz(baz: Option[String]) }

    case class Bar(baz: Bar.Baz)
    object Bar { case class Baz(baz: String) }
    
    Foo(Foo.Baz(Some("baz"))).transformIntoPartial[Bar].asEither.left.map(_.asErrorPathMessages)  // Right(Baz(Bar.Baz("baz")))
    Foo(Foo.Baz(None)).transformIntoPartial[Bar].asEither.left.map(_.asErrorPathMessages) // Left(Iterable("baz.bar" -> EmptyValue))
    ```

Examples so far assumed, that each constructor's argument was paired with a field of exactly the same name. So, let's
show what to do if that isn't the case.

### Reading from methods

If we want to read from `def fieldName: A` as if it was `val fieldName: A` - which could be unsafe as it might perform
side effects - you need to enable the `.enableMethodAccessors` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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
      implicit val cfg = TransformerConfig.default.enableMethodAccessors
      
      (new Source("value", 512)).transformInto[Target]
      // val source = new Source("value", 512)
      // new Target(source.a, source.b())
      (new Source("value", 512)).transformIntoPartial[Target]
      // val source = new Source("value", 512)
      // partial.Result.fromValue(new Target(source.a, source.b()))
    }
    ```

Flag `.enableMethodAccessors` will allow macros to consider methods which are:

  - nullary (take 0 value arguments)
  - have no type parameters
  - cannot be considered Bean getters

If the flag was enabled in implicit config it can be disabled with `.disableMethodAccessors`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(_a: String, _b: Int) {
      def a: String = _a
      def b(): Int = _b
    }
    class Target(a: String, b: Int)
    
    implicit val cfg = TransformerConfig.default.enableMethodAccessors
    
    (new Source("value", 512)).into[Target].disableMethodAccessors.transform // compilation fails
    ```

### Reading from inherited values/methods

Out of the box, only values defined directly within the source type are considered. If we want to read from `val`s
inherited from a source value's supertype, you need to enable the `.enableInheritedAccessors` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    trait Parent {
      val a = "value"
    }
    case class Source(b: Int) extends Parent
    case class Target(a: String, b: Int)
    
    Source(10).into[Target].enableInheritedAccessors.transform // Bar("value", 10)
    Source(10).intoPartial[Target].enableInheritedAccessors.transform.asEither // Right(Bar("value", 10))
    
    locally {
      implicit val cfg = TransformerConfig.default.enableInheritedAccessors
      
      Source(10).transformInto[Target] // Bar("value", 10)
      Source(10).transformIntoPartial[Target].asEither // Right(Bar("value", 10))
    }
    ```

!!! tip

    `.enableInheritedAccessors` can be combined with [`.enableMethodAccessors`](#reading-from-inherited-valuesmethods)
    and [`.enableBeanGetters`](#reading-from-bean-getters) to allow reading from inherited `def`s and Bean getters.

If the flag was enabled in implicit config it can be disabled with `.enableInheritedAccessors`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    trait Parent {
      val a = "value"
    }
    case class Source(b: Int) extends Parent
    case class Target(a: String, b: Int)
    
    implicit val cfg = TransformerConfig.default.enableInheritedAccessors
    
    Source(10).into[Target].disableInheritedAccessors.transform
    ```

### Reading from Bean getters

If we want to read `def getFieldName(): A` as if it was `val fieldName: A` - which would allow reading from Java Beans
(or Plain Old Java Objects) - you need to enable a flag: 

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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
      implicit val cfg = TransformerConfig.default.enableBeanGetters
      
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

If the flag was enabled in implicit config it can be disabled with `.disableBeanGetters`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(a: String, b: Int) {
      def getA(): String = a
      def getB(): Int = b
    }
    class Target(a: String, b: Int)
    
    implicit val cfg = TransformerConfig.default.enableBeanGetters
    
    (new Source("value", 512)).into[Target].disableBeanGetters.transform // compilation fails
    ```

### Writing to Bean setters

If we want to write to `def setFieldName(fieldName: A): Unit` as if it was `fieldName: A` argument of a constructor -
which would allow creating from Java Beans (or Plain Old Java Objects) - you need to enable the `.enableBeanSetters`
flag: 

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(val a: String, val b: Int)
    class Target() {
      private var a = ""
      private var b = 0
    }
    
    (new Source("value", 512)).into[Target].enableBeanSetters.transform
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.setA(source.a)
    // target.setA(source.b)
    // target
    (new Source("value", 512)).intoPartial[Target].enableBeanSetters.transform
    // val source = new Source("value", 512)
    // val target = new Target()
    // target.setA(source.a)
    // target.setA(source.b)
    // partial.Result.fromValue(target)
    
    locally {
      implicit val cfg = TransformerConfig.default.enableBeanSetters
      
      (new Source("value", 512)).transformInto[Target]
      // val source = new Source("value", 512)
      // val target = new Target()
      // target.setA(source.a)
      // target.setA(source.b)
      // target
      (new Source("value", 512)).transformIntoPartial[Target]
      // val source = new Source("value", 512)
      // val target = new Target()
      // target.setA(source.a)
      // target.setA(source.b)
      // partial.Result.fromValue(target)
    }
    ```

Flag `.enableBeanSetters` will allow macros to write to methods which are:

  - unary (take 1 value arguments)
  - have no type parameters
  - have names starting with `set` - for comparison `set` will be dropped and the first remaining letter lowercased

_besides_ calling constructor (so you can pass values to _both_ the constructor and setters at once). Without the flag
macro will fail compilation to avoid creating potentially uninitialized object.

If the flag was enabled in implicit config it can be disabled with `.disableBeanSetters`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    class Source(val a: String, val b: Int)
    class Target() {
      private var a = ""
      private var b = 0
    }
    
    implicit val cfg = TransformerConfig.default.enableBeanSetters
    
    (new Source("value", 512)).into[Target].disableBeanSetters.transform // compilation fails
    ```

### Allowing the constructor's default values

When calling constructor manually, sometimes we want to not pass all arguments ourselves and let the default values
handle the remaining ones. If Chimney did it out of the box, it could lead to some subtle bugs - you might prefer
compilation error reminding you to provide the value yourself - but if you know that it is safe you can enable fallback
to default values with the `.enableDefaultValues` flag:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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

If the flag was enabled in implicit config it can be disabled with `.disableDefaultValues`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Source(a: String, b: Int)
    case class Target(a: String, b: Int = 0, c: Long = 0L)
    
    implicit val cfg = TransformerConfig.default.enableDefaultValues
    
    (new Source("value", 512)).into[Target].disableDefaultValues.transform // compilation fails
    ```

### Wiring the constructor's parameter to its source field

In some cases there is no source field available of the same name as the constructor's argument. However, other field
could be used in this role. Other time the source field of the matching name exists, but we want to explicitly override
it with another field. Since the usual cause of such cases is a _rename_, we can handle it using `.withFieldRenamed`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String, b: Int)
    case class Bar(a: String, c: Int)
    
    Foo("value", 1248).into[Bar].withFieldRenamed(_.b, _.c).transform // Bar("value", 1248)
    Foo("value", 1248).intoPartial[Bar].withFieldRenamed(_.b, _.c).transform.asEither // Right(Bar("value", 1248))
    ```

The requirements to use a rename are as following:

  - you have to pass `_.fieldName` directly, it cannot be done with a reference to function
  - the field rename is _flat_, you cannot pass `_.foo.bar.baz` there
  - you can only use `val`/nullary method/Bean getter as a source field name
  - you have to have a `val`/nullary method/Bean getter with a name matching constructor's argument (or Bean setter if
    setters are enabled)
 
The last 2 conditions are always met when working with `case class`es with no `private val`s in constructor, and classes
with all arguments declared as public `val`s, and Java Beans where each setter has corresponding getter defined.

!!! example

    Field renaming with Java Beans 

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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

    implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters
    
    (new Foo()).into[Bar].withFieldRenamed(_.getB, _.getC).transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA) 
    // bar.setC(foo.getB())
    // bar
    (new Foo()).intoPartial[Bar].withFieldRenamed(_.getB, _.getC).transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA) 
    // bar.setC(foo.getB())
    // partial.Result.fromValue(bar)
    ```

### Wiring the constructor's parameter to a raw value

Another way of handling the missing source field - or overriding existing one - is providing the value for 
the constructor's argument/setter yourself. The successful value can be provided with `.withFieldConst`:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    
    case class Foo(a: String, b: Int)
    case class Bar(a: String, b: Int, c: Long)
    
    // successful partial.Result constant
    Foo("value", 10).into[Bar].withFieldConstPartial(_.c, partial.Result.fromValue(100L)).transform
      .asEither.left.map(_.asErrorPathMessages) // Right(Bar("value", 10, 1000L))
    // a few different partial.Result failures constants
    Foo("value", 10).into[Bar].withFieldConstPartial(_.c, partial.Result.fromEmpty).transform
      .asEither.left.map(_.asErrorPathMessages) // Left(Iterable("c", partial.Error.EmptyMessage))
    Foo("value", 10).into[Bar].withFieldConstPartial(_.c, partial.Result.fromThrowable(new NullPoinerException)).transform
      .asEither.left.map(_.asErrorPathMessages) // Left(Iterable("c", partial.Error.ThrowableMessage(e: NullPoinerException)))
    Foo("value", 10).into[Bar].withFieldConstPartial(_.c, partial.Result.fromErrorString("bad value")).transform
      .asEither.left.map(_.asErrorPathMessages) // Left(Iterable("c", partial.Error.StringMessage("bad value")))
    ``` 

As you can see, the transformed value will automatically preserve the field name for which failure happened.

The requirements to use a value provision are as following:

  - you have to pass `_.fieldName` directly, it cannot be done with a reference to function
  - the field rename is _flat_, you cannot pass `_.foo.bar.baz` there
  - you have to have a `val`/nullary method/Bean getter with a name matching constructor's argument (or Bean setter if
    setters are enabled)

The last conditions is always met when working with `case class`es with no `private val`s in constructor, and classes
with all arguments declared as public `val`s, and Java Beans where each setter has corresponding getter defined.

!!! example

    Value provision with Java Beans 

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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

    implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters
    
    (new Foo()).into[Bar].withFieldConst(_.getC, 100L).transform
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

### Wiring the constructor's parameter to computed value

Yet another way of handling the missing source field - or overriding existing one - is computing the value for 
the constructor's argument/setter out from a whole transformed value. The always-succeeding transformation can be provided
with `.withFieldComputed`:

!!! example 

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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
      .transform.asEither.left.map(_.asErrorPathMessages)// Left(Iterable("c", partial.Error.StringMessage("bad value")))
    // failure depends on the input (whether .toLong throws or not)
    Foo("20", 10).intoPartial[Bar]
      .withFieldComputedPartial(_.c, foo => partial.Result.fromCatching(foo.a.toLong))
      .transform.asEither.left.map(_.asErrorPathMessages)// Right(Bar("20", 10, 20L))
    Foo("value", 10).intoPartial[Bar]
      .withFieldComputedPartial(_.c, foo => partial.Result.fromCatching(foo.a.toLong))
      .transform.asEither.left.map(_.asErrorPathMessages)// Left(Iterable("c", partial.Error.ThrowableMessage(e: NumberFormatException)))
    ``` 

As you can see, the transformed value will automatically preserve the field name for which failure happened.

The requirements to use a value computation are as following:

  - you have to pass `_.fieldName` directly, it cannot be done with a reference to function
  - the field rename is _flat_, you cannot pass `_.foo.bar.baz` there
  - you have to have a `val`/nullary method/Bean getter with a name matching constructor's argument (or Bean setter if
    setters are enabled)

The last conditions is always met when working with `case class`es with no `private val`s in constructor, and classes
with all arguments declared as public `val`s, and Java Beans where each setter has corresponding getter defined.

!!! example

    Value computation with Java Beans 

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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

    implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters
    
    (new Foo()).into[Bar].withFieldComputed(_.getC, foo => foo.getB().toLong).transform
    // val foo = new Foo()
    // val bar = new Bar()
    // bar.setA(foo.getA) 
    // bar.setC(100L)
    // bar
    (new Foo()).intoPartial[Bar].withFieldComputedPartial(_.getC, foo => partial.Result.fromCatched(foo.getA.toLong)).transform
    // val foo = new Foo()
    // partial.Result.fromCatched(foo.getA.toLong).map { c =>
    //   val bar = new Bar()
    //   bar.setA(foo.getA) 
    //   bar.setC(c)
    //   bar
    // }
    ```

## From/into a `Tuple`

Conversion from/to a tuple of any size is almost identical to conversion between other classes. The only difference
is that when either source or target type is a tuple, automatic matching between source field and target constructor's
argument is made by position instead of name:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String, b: Int, c: Long)
    
    Foo("value", 42, 1024L).transformInto[(String, Int, Long)] // ("value", 42, 1024L) 
    ("value", 42, 1024L).transformInto[Foo] // Foo("value", 42, 1024L)
    Foo("value", 42, 1024L).transformIntoPartial[(String, Int, Long)] // Right(("value", 42, 1024L)) 
    ("value", 42, 1024L).transformIntoPartial[Foo] // Right(Foo("value", 42, 1024L))
    ```

!!! tip

    You can use all the flags, renames, value provisions and computations that are available to case classes, Java Beans
    and so on.

## From/into an `AnyVal`

`AnyVal`s can be used both as data sources for derivation as well as the targets of the transformation.

If `AnyVal` is the source, Chimney would attempt to unwrap it, and if it's the target wrap it - we basically treat
`AnyVal`s as transparent, similarly to virtually every other Scala library.

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: Int) extends AnyVal
    case class Bar(b: String) extends AnyVal
    
    Foo(10).into[Bar].transform // Bar(10)
    Foo(10).transformInto[Bar] // Bar(10)
    Foo(10).transformIntoPartial[Bar].asEither // Right(Bar(10))
    Foo(10).intoPartial[Bar].transform.asEither // Right(Bar(10))
    ```

!!! tip

    This behavior is non-configurable in Chimney, similarly like it is non-configurable in every other library. If you
    decided to use a derivation then libraries will wrap and upwrap `AnyVal`s for you automatically.
    
    If you don't want this behavior you can prevent it (in every library, not only Chimney) by making the `val`
    `private` - to prevent unwrapping - and/or making the constructor `private` - to prevent wrapping. This way you'd
    have to provide support for you type for each library by yourself.
    
    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(private a: Int) extends AnyVal // cannot be automatically unwrapped
    case class Bar private (b: String) extends AnyVal // cannot be automatically wrapped
    
    Foo(10).transformInto[Bar] // compilation fails
    ```

!!! tip

    When `AnyVal` special handling cannot be used (e.g. because value/constructor is private), then Chimney falls back
    to treating them as normal class.

## Between `sealed`/`enum`s

When both the source type and the target type of the transformation are `sealed` (`trait`, `abstract class`), Chimney
will convert the source type's subtypes into the target type's subtypes. To make it work out of the box, every source
type's subtype needs to have a corresponding subtype in target type with a matching name:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    sealed trait Foo
    object Foo {
      case class Baz(a: String, b: Int) extends Foo
      case object Buzz extends Foo  
    }
    sealed trait Bar
    object Bar {
      case class Baz(a: Int) extends Bar
      case object Fizz extends Bar  
      case object Buzz extends Bar
    }
    
    (Foo.Baz("value", 10): Foo).transformInto[Bar] // Bar.Baz(10)
    (Foo.Baz("value", 10): Foo).into[Bar].transform // Bar.Baz(10)
    (Foo.Buzz: Foo).transformInto[Bar] // Bar.Buzz
    (Foo.Buzz: Foo).into[Bar].transform // Bar.Buzz
    (Foo.Baz("value", 10): Foo).transformIntoPartial[Bar] // Bar.Baz(10)
    (Foo.Baz("value", 10): Foo).intoPartial[Bar].transform // Bar.Baz(10)
    (Foo.Buzz: Foo).transformIntoPartial[Bar] // Bar.Buzz
    (Foo.Buzz: Foo).intoPartial[Bar].transform // Bar.Buzz
    ```

It works also with Scala 3's `enum`:

!!! example

    `sealed trait` into `enum`

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl.*
    
    sealed trait Foo
    object Foo:
      case class Baz(a: String, b: Int) extends Foo
      case object Buzz extends Foo
    enum Bar:
      case Baz(a: Int)
      case Fizz  
      case Buzz
    
    (Foo.Baz("value", 10): Foo).transformInto[Bar] // Bar.Baz(10)
    (Foo.Buzz: Foo).transformInto[Bar] // Bar.Buzz
    ```
    
!!! example

    `enum` into `sealed trait` 

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl.*
    
    enum Foo:
      case Baz(a: String, b: Int)
      case Buzz
    sealed trait Bar
    object Bar:
      case class Baz(a: Int) extends Bar
      case object Fizz extends Bar  
      case object Buzz extends Bar
    
    (Foo.Baz("value", 10): Foo).transformInto[Bar] // Bar.Baz(10)
    (Foo.Buzz: Foo).transformInto[Bar] // Bar.Buzz
    ```
    
!!! example

    `enum` into `enum` 

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl.*
    
    enum Foo:
      case Baz(a: String, b: Int)
      case Buzz
    enum Bar:
      case Baz(a: Int)
      case Fizz  
      case Buzz
    
    (Foo.Baz("value", 10): Foo).transformInto[Bar] // Bar.Baz(10)
    (Foo.Buzz: Foo).transformInto[Bar] // Bar.Buzz
    ```

To enable seamless work with [Protocol Buffers](cookbook.md#protocol-buffers-integration), there is also a special
handling for non-flat ADTs, where each subtype of a `sealed`/`enum` is a single-parameter wrapper around a `case class`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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
    (protobuf.A(Foo.A("value", 42)) : protobuf.Foo).transformInto[domain.Bar] // domain.Bar.A("value", 42)
    (protobuf.B(Foo.B()) : protobuf.Foo).transformInto[domain.Bar] // domain.Bar.B
    // unflattening
    (domain.Bar.A("value", 42): domain.Bar).transformInto[protobuf.Foo] // protobuf.A(Foo.A("value", 42)
    (domain.Bar.B : domain.Bar).transformInto[protobuf.Foo] // protobuf.B(Foo.B())
    ```

However, Java's `enum` can also be converted this way to/from `sealed`/Scala 3's `enum`/another Java's `enum`:

!!! example

    Java's `enum` to/from `sealed`

    ```java
    // in Java
    enum ColorJ {
      Red, Green, Blue;
    }
    ```

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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

### Handling a specific `sealed` subtype with a raw value

TODO

TODO java enums limitations

### Handling a specific `sealed` subtype with a computed value

TODO

TODO java enums limitations

## From/into an `Option`

`Option` type have a special support during derivation of a transformation.

The transformation from one `Option` into another is obviously always supported:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
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
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String)
    
    Foo("value").transformInto[Option[Bar]] // Some(Bar("value"))
    Foo("value").into[Option[Bar]].transform // Some(Bar("value"))
    Foo("value").transformIntoPartial[Option[Bar]].asEither // Right(Some(Bar("value")))
    Foo("value").intoPartial[Option[Bar]Bar].transform.asEither // Right(Some(Bar("value")))
    ```

However, unwrapping of an `Option` is impossible without handling `None` case, that's why Chimney handles it
automatically only with `PartialTransformer`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String)
    
    Option(Foo("value")).transformIntoPartial[Bar].asEither // Right(Bar("value"))
    (None : Option[Foo]).transformIntoPartial[Bar].asEither // Left(...) TODO
    Option(Foo("value")).intoPartial[Bar].transform.asEither // Right(Bar("value"))
    (None : Option[Foo]).intoPartial[Bar].transform.asEither // Left(...) TODO
    ```

!!! tip

    Out of the box, Chimney supports only Scala's build-in `Option`s.
    
    If you need to integrate with Java's `Optional`, please, read about
    [Java's collections integration](cookbook.md#java-collections-integration).
    
    If you need to provide support for your own optional types, please, read about
    [custom optional types](cookbook.md#custom-optional-types).

### Allowing `None` as the constructor's argument's fallback

Sometimes we are transforming value into a type which would use `Option`'s `None` to handle some default behavior and
`Some` as the user's overrides. This type might not have default value (e.g. `value: Option[A] = None`) in its
constructor, but we would find it useful to fallback on `None` in such cases. It is not enabled out of the box, for
similar reasons to default values support, but we can enable it with the `.enableOptionDefaultsToNone` flag:   

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String, b: Option[String] = Some("a"))
    
    // without flags -> compilation error
    Foo("value").into[Bar].enableOptionDefaultsToNone.transform // Bar("value", None)
    Foo("value").intoPartial[Bar].enableOptionDefaultsToNone.transform.asOption // Some(Bar("value", None))
    
    locally {
      implicit val cfg = TransformerConfig.default.enableOptionDefaultsToNone
      
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
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String, b: Option[String] = Some("a"))
    
    Foo("value").into[Bar].enableDefaultValues.enableOptionDefaultsToNone.transform // Bar("value", Some("a"))
    Foo("value").intoPartial[Bar].enableDefaultValues.enableOptionDefaultsToNone.transform.asOption // Some(Bar("value", Some("a")))
    
    locally {
      implicit val cfg = TransformerConfig.default.enableOptionDefaultsToNone.enableDefaultValues
      
      Foo("value").transformInto[Bar] // Bar("value", Some("a"))
      Foo("value").transformIntoPartial[Bar].asOption // Some(Bar("value", Some("a")))
    }
    ```
    
    The original default value has a higher priority than `None`.

If the flag was enabled in implicit config it can be disabled with `.disableOptionDefaultsToNone`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String, b: Option[String] = Some("a"))

    implicit val cfg = TransformerConfig.default.enableOptionDefaultsToNone
    
    Foo("value").into[Bar].disableOptionDefaultsToNone.transform // compilation error
    ```

## Between `Either`s

Transformation from one `Either` to another is supported as long as both left and right type can also be converted:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    case class Foo(a: String)
    case class Bar(a: String)
    
    (Left(Foo("value")) : Either[Foo, Bar]).transformInto[Either[Bar, Foo]] // Left(Bar("value"))
    (Right(Bar("value")) : Either[Foo, Bar]).transformInto[Either[Bar, Foo]] // Right(Foo("value"))
    (Left(Foo("value")) : Either[Foo, Bar]).transformIntoPartial[Either[Bar, Foo]].asOption // Some(Left(Bar("value")))
    (Right(Bar("value")) : Either[Foo, Bar]).transformIntoPartial[Either[Bar, Foo]].asOption // Some(Right(Foo("value")))
    ```

Transformation from `Left` and `Right` into `Either` requires existence of only the transformation from the type we
know for sure is inside to their corresponding type in target `Either`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
     
    case class Foo(a: String, b: Int)
    case class Bar(a: String)
    case class Baz(a: String, b: Int, c: Long)

    // Foo -> Bar - can be derived
    // Foo -> Baz - cannot be derived without providing c
    (Left(Foo("value"))).transformInto[Either[Bar, Baz]] // Left(Bar("value"))
    (Right(Foo("value"))).transformInto[Either[Baz, Bar]] // Right(Bar("value"))
    ```

## Between Scala's collections/`Array`s

Every `Array`/every collection extending `scala.collection.Iterable` can be used as a source value for a collection's
transformation.

Every `Array`/every collection provided with `scala.collection.compat.Factory` can be used as a target type for a
collection's transformation.

The requirement for collection's transformation is that both source's and target's condition are met and that types
stored within these collections can also be converted. 

!!! example

    ```scala
    //> using dep io.scalaland::chimney:{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
     
    TODO
    ```

TODO: partial examples for iterables and maps

!!! tip

    Out of the box, Chimney supports only Scala's build-in collections, which are extending `Iterable` and have
    `scala.collection.compat.Factory` provided as an implicit.
    
    If you need to integrate with Java's collections, please, read about
    [Java's collections integration](cookbook.md#java-collections-integration).
    
    If you need to provide support for your own collection types, you have to write your own implicit methods. 

## Custom transformations

TODO

TODO total -> partial (partial can use total)

### Resolving priority of implicit Total vs Partial Transformers

TODO implicit conflict resolution

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

TODO: check if we reject implicit/upcast on override

TODO if we do add warning to the sections above

TODO: recursive types and .define and .derive
