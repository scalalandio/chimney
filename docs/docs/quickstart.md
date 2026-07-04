# Quick Start

Chimney is supported for Scala **2.13**, **3.8.4+** on [**JVM**](https://www.scala-lang.org/),
[**Scala.js**](https://www.scala-js.org/) and [**Scala Native**](https://scala-native.org/) with full feature parity
between each version.

(Scala **3.3+** was supported on the `1.x` line, **2.12** for whole `1.x` line, and **2.11** on `0.5.x` line)

!!! warning "JDK requirements"

    On the JVM, since `2.0.0`, Chimney's Scala **2.13** artifacts require **JDK 11+** and Scala **3** artifacts require
    **JDK 17+** (the macros are built on top of [Hearth](https://scala-hearth.readthedocs.io/), which is JDK 11+).
    Scala.js and Scala Native are unaffected (but compilation still happens on a JVM meeting these requirements).

The newest stable versions on each platform are:

[![Chimney JVM versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=jvm)](https://search.maven.org/artifact/io.scalaland/chimney_2.13) <br>
[![Chimney Scala.js 1.x versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=sjs1)](https://search.maven.org/artifact/io.scalaland/chimney_sjs1_2.13) <br>
[![Chimney Scala.js 0.6 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=sjs0.6)](https://search.maven.org/artifact/io.scalaland/chimney_sjs0.6_2.13) <br>
[![Chimney Scala Native 0.5 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.5)](https://search.maven.org/artifact/io.scalaland/chimney_native0.5_2.13) <br>
[![Chimney Scala Native 0.4 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.4)](https://search.maven.org/artifact/io.scalaland/chimney_native0.4_2.13) <br>
[![Chimney Scala Native 0.3 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.3)](https://search.maven.org/artifact/io.scalaland/chimney_native0.3_2.11) <br>

with newest Scaladoc API documentation available:

[![Scaladoc 2.11](https://javadoc.io/badge2/io.scalaland/chimney_2.11/scaladoc%202.11.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.11)
[![Scaladoc 2.12](https://javadoc.io/badge2/io.scalaland/chimney_2.12/scaladoc%202.12.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.12)
[![Scaladoc 2.13](https://javadoc.io/badge2/io.scalaland/chimney_2.13/scaladoc%202.13.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.13)
[![Scaladoc 3](https://javadoc.io/badge2/io.scalaland/chimney_3/scaladoc%203.svg)](https://javadoc.io/doc/io.scalaland/chimney_3)

To start using the library add to your sbt config:

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney" % "{{ chimney_version() }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney" % "{{ chimney_version() }}"
```

or try it in [Scala CLI](https://scala-cli.virtuslab.org/):

```bash
# Scala 2.13
scala-cli repl --scala "{{ scala.2_13 }}" --dependency "io.scalaland::chimney::{{ chimney_version() }}"
# Scala 3
scala-cli repl --scala "{{ scala.3 }}" --dependency "io.scalaland::chimney::{{ chimney_version() }}"
```

then import in your codebase:

```scala
import io.scalaland.chimney.dsl._
```

and you are good to go!

```scala
case class User(id: UUID, name: String, surname: String)
case class ApiUser(name: String, surname: String)

val userID: UUID = ...
val user: User = ...

// Use .transformInto[Type], when don't need to customize anything...:
val apiUser: ApiUser  = user.transformInto[ApiUser]

// ...and .into[Type].customization.transform when you do:
val user2: User = apiUser.into[User].withFieldConst(_.id, userID).transform

// If yout want to reuse some Transformation (and you don't want to write it by hand)
// you can generate it with .derive:
implicit val transformer: Transformer[ApiUser, User] = Transformer.derive[ApiUser, User]

// ...or with .define.customization.buildTransformer:
implicit val transformerWithOverrides: Transformer[User, ApiUser] = Transformer.define[User, ApiUser]
  .withFieldConst(_.id, userID)
  .buildTransformer

// It works the same way with PartialTransformers and Patchers.
```

Chimney will take care of generating the boring transformation code, and if it finds something non-obvious, it will give
you a nice error message what it needs:  

```scala
apiUser.transformInto[User]
// Chimney can't derive transformation from ApiUser to User
//
// User
//   id: java.util.UUID - no accessor named id in source type ApiUser
//
// Consult https://chimney.readthedocs.io for usage examples.
```

But don't you worry! Usually Chimney only needs your help if there is no field in the source value with a matching name
or whe the targeted type has a private constructor. Out of the box, it supports:

 * conversions [between `case class`es](supported-transformations.md#into-a-case-class-or-pojo)
    * actually, a conversion between *any* `class` and *another `class` with a public constructor*
    * with [an opt-in support for Java Beans](supported-transformations.md#reading-from-bean-getters)
 * conversions [between `sealed trait`s, Scala 3 `enum`s, Java `enum`s](supported-transformations.md#between-sealedenums)
 * conversions [between collections](supported-transformations.md#between-scalas-collectionsarrays)
 * conversions [between `Option`s](supported-transformations.md#frominto-an-option)
 * conversions [between `Either`s](supported-transformations.md#between-eithers)
 * [wrapping/unwrapping `AnyVal`s](supported-transformations.md#frominto-an-anyval)
 * conversions where [some transformation can fail in runtime](supported-transformations.md#total-transformers-vs-partialtransformers)
   (parsing, smart constructors)
 * [mergings multiple `case class`es or tuples into one](supported-transformations.md#merging-multiple-input-sources-into-a-single-target-value)
    * allowing combining of [`Option`s](supported-transformations.md#merging-option-with-option-into-option)
    * allowing combining of [`Either`s](supported-transformations.md#merging-either-with-either-into-either)
    * allowing combining of [collections](supported-transformations.md#merging-collection-with-collection-into-collection)
 * [patching one `case class` with another](supported-patching.md)
    * with special handling of [`Option`s](supported-patching.md#updating-value-with-option)
    * with special handling of [`Either`s](supported-patching.md#updating-value-with-either)
    * with special handling of [collections](supported-patching.md#updating-value-with-collection)
 * [previewing how Chimney attempts to generate the transformation](troubleshooting.md#debugging-macros) 

[And much, much more!](supported-transformations.md)

!!! tip

    If you are looking for videos or a tutorials take a look at
    [More sources, videos and tutorials](troubleshooting.md#more-sources-videos-and-tutorials) section!

!!! tip

    If you are an advanced user, who wants to learn the difference between automatic derivation and semiautomatic derivation
    in Chimney, see [Automatic, semiautomatic and inlined derivation](cookbook.md#automatic-semiautomatic-and-inlined-derivation).

## Java collections integration

If you are interested in using `java.util.Optional`, `java.util.Collection`s, `java.util.Map`s, `java.util.stream`s,
Java boxed primitives (`java.lang.Integer` and friends) and other Java's types - since `2.0.0` they are supported
out of the box on the JVM. No extra dependency, no import needed (the `chimney-java-collections` artifact is gone).

!!! tip

    See [Java collections integration cookbook](cookbook.md#java-collections-integration) for more information.

## Cats integration

If you are interested in Cats type class instances for Chimney's types (and conversions between `partial.Result`
and `Validated`), you need to add to your project:

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney-cats" % "{{ chimney_version() }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney-cats" % "{{ chimney_version() }}"
```

```scala
import io.scalaland.chimney.cats._
```

Conversions to/from `cats.data` collections (`NonEmptyList`, `Chain`, ...) are since `2.0.0` served by a separate
library - [Kindlings](https://github.com/kubuszok/kindlings)' `kindlings-cats-integration` - which only needs
to be added to the classpath (no import):

```scala
libraryDependencies += "com.kubuszok" %%% "kindlings-cats-integration" % "{{ libraries.kindlings }}"
```

!!! tip

    See [Cats integration cookbook](cookbook.md#cats-integration) for more information.

## Protocol Buffers integration

If you want to improve support for types defined in ScalaPB, you need to add to your project:

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney-protobufs" % "{{ chimney_version() }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney-protobufs" % "{{ chimney_version() }}"
```

Since `2.0.0` having the artifact on the classpath is enough for the ScalaPB well-known types (`wrappers.*Value`,
`ByteString`, `Timestamp`). The import:

```scala
import io.scalaland.chimney.protobufs._
```

is only needed for the remaining utilities (`Empty`, the `Duration` family, empty `oneof` handling,
`UnrecognizedEnum`, `DefaultValue[UnknownFieldSet]`).

!!! tip

    See [Protocol Buffers integration cookbook](cookbook.md#protocol-buffers-integration) for more information.
