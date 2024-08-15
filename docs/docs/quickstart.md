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

and then import in your codebase:

```scala
import io.scalaland.chimney.dsl._
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

!!! tip

    If you are looking for videos or a tutorials take a look at
    [More sources, videos and tutorials](troubleshooting.md#more-sources-videos-and-tutorials) section!

!!! tip

    If you are a advanced user, who wants to learn the difference between automatic derivation and semiautomatic derivation
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
