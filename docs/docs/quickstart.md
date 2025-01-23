# Quick Start

Chimney is supported for Scala **2.12**, **2.13**, **3.3+** on [**JVM**](https://www.scala-lang.org/),
[**Scala.js**](https://www.scala-js.org/) and [**Scala Native**](https://scala-native.org/) with full feature parity
between each version.

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
# Scala 2.12
scala-cli repl --scala "{{ scala.2_12 }}" --dependency "io.scalaland::chimney::{{ chimney_version() }}"
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

// Use .transformInto[Type], when don't need to customize anything... 
val apiUser: ApiUser  = user.transformInto[ApiUser]

// ...and .into[Type].customization.transform when you do.
val user2: User = apiUser.into[User].withFieldConst(_.id, userID).transform
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

 * conversions [between `case class`es](supported-transformations.md#into-a-case-class)
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

If you are interested in using `java.util.Optional`, `java.util.Collection`s, `java.util.Map`s, `java.util.streams` and
other Java's types, you need to add integration to your project:

```scala
// Java collections integrations is released only on JVM Scala!
libraryDependencies += "io.scalaland" %% "chimney-java-collections" % "{{ chimney_version() }}"
```

and then import:

```scala
import io.scalaland.chimney.javacollections._
```

!!! tip

    See [Java collections integration cookbook](cookbook.md#java-collections-integration) for more information.

## Cats integration

If you are interested in Cats integrations for Partial Transformers, you need to add to your project:

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney-cats" % "{{ chimney_version() }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney-cats" % "{{ chimney_version() }}"
```

```scala
import io.scalaland.chimney.cats._
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

```scala
import io.scalaland.chimney.protobufs._
```

!!! tip

    See [Protocol Buffers integration cookbook](cookbook.md#protocol-buffers-integration) for more information.
