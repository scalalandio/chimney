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
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
    import io.scalaland.chimney.dsl._

    trait A
    class B
    val b = new B
   
    b.transformInto[A]  // == (b: A)
    b.into.transform[A] // == (b: A)
    b.transformIntoPartial[A].asEither  // == Right(b: A)
    b.intoPartial[A].transform.asEither // == Right(b: A)
    ```

## Into `case class` (or POJO)

Every type can have its `val`s read as used as data sources for transformation.

And every class with a public primary constructor can be the target of the transformation.

To make it work out of the box, every argument of a constructor needs to be paired with a matching field (`val`) in the
transformed value.

The obvious example are `case class`es with the same fields:

!!! example

    ```scala
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
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
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
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
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
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
    //> using dep "io.scalaland::chimney:{{ git.tag or local.tag }}"
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

    ```scala
    // TODO: example of failing field transformation with a path
    ```

Examples so far assumed, that each constructor's argument was paired with a field of exactly the same name. So, let's
show what to do if that isn't the case.

### Reading from Bean getters

```scala
// TODO example
bean.into[CaseClass].enableBeanGetters.transform
```

### Writing to Bean setters

```scala
// TODO example
bean.into[CaseClass].enableBeanSetters.transform
```

### Allowing constructor's defaults

### Wiring constructor's parameter to its source field

TODO mentions how it works in Java Beans

### Wiring constructor's parameter to raw value

TODO mentions how it works in Java Beans

### Wiring constructor's parameter to computed value

TODO mentions how it works in Java Beans

## From/To Tuple

TODO

## From/To `AnyVal`

TODO

TODO tip about fallback behavior, non configurability and private constructor 

## Between `sealed`/`enum`s

TODO

TODO java enums

TODO flags

TODO overrides

TODO java enums limitations

## From/To `Option`

TODO

TODO option unwrapping in partial

TODO refer Optional support in java-collections

TODO flags

## Between `Either`s

## Between Scala's collections

TODO

TODO mention Factory

TODO mention java-collections

## Custom transformations

TODO

TODO total -> partial

TODO implicit conflict resolution
