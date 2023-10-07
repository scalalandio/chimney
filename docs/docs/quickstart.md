# Quickstart

Chimney is supported for Scala 2.12, 2.13, 3.3+ on JVM, Scala.js and Scala Native with full feature parity between each
version.

Newest versions on each platform are:

[![Chimney JVM versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=jvm)](https://search.maven.org/artifact/io.scalaland/chimney_2.13) <br>
[![Chimney Scala.js 1.x versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=sjs1)](https://search.maven.org/artifact/io.scalaland/chimney_sjs1_2.13) <br>
[![Chimney Scala.js 0.6 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=sjs0.6)](https://search.maven.org/artifact/io.scalaland/chimney_sjs0.6_2.13) <br>
[![Chimney Scala Native 0.4 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.4)](https://search.maven.org/artifact/io.scalaland/chimney_native0.4_2.13) <br>
[![Chimney Scala Native 0.3 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.3)](https://search.maven.org/artifact/io.scalaland/chimney_native0.3_2.11) <br>

with Scaladoc API documentation available:

[![Scaladoc 2.11](https://javadoc.io/badge2/io.scalaland/chimney_2.11/scaladoc%202.11.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.11)
[![Scaladoc 2.12](https://javadoc.io/badge2/io.scalaland/chimney_2.12/scaladoc%202.12.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.12)
[![Scaladoc 2.13](https://javadoc.io/badge2/io.scalaland/chimney_2.13/scaladoc%202.13.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.13)
[![Scaladoc 3](https://javadoc.io/badge2/io.scalaland/chimney_3/scaladoc%203.svg)](https://javadoc.io/doc/io.scalaland/chimney_3)

To start using the library add to your sbt config:

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney" % "{{ git.tag or scm.latest }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney" % "{{ git.tag or scm.latest }}"
```

and then import in your codebase:

```scala
import io.scalaland.chimney.dsl._
```

or try it in Scala CLI:

```bash
# Scala 2.12
scala-cli repl --scala "2.12.18" --dependency "io.scalaland::chimney:{{ git.tag or scm.latest }}"
# Scala 2.13
scala-cli repl --scala "2.13.12" --dependency "io.scalaland::chimney:{{ git.tag or scm.latest }}"
# Scala 3
scala-cli repl --scala "3.3.1" --dependency "io.scalaland::chimney:{{ git.tag or scm.latest }}"
```

If you are advanced used who want to learn about the difference about automatic derivation and semiautomatic derivation
see [Automatic, semiautomatic and inlined derivation](/cookbook/#automatic-semiautomatic-and-inlined-derivation).

## Java collections integration

If you are interested in using `java.util.Optional`, `java.util.Collection`s, `java.util.Map`s, `java.util.streams` and
other Java's types, you need to add integration to your project:

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney-java-collections" % "{{ git.tag or scm.latest }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney-java-collections" % "{{ git.tag or scm.latest }}"
```

and then import:

```scala
import io.scalaland.chimney.javacollections._
```

See [Java collections integration cookbook](/cookbook/#java-collections-integration) for more information.

## Cats integration

If you are interested in Cats integrations for Partial Transformers, you need add to your project:

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney-cats" % "{{ git.tag or scm.latest }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney-cats" % "{{ git.tag or scm.latest }}"
```

```scala
import io.scalaland.chimney.cats._
```

See [Cats integration cookbook](/cookbook/#cats-integration) for more information.

## Protocol Buffers integration

If you want to improve support for types defined in ScalaPB, you need to add to your project:

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney-protobufs" % "{{ git.tag or scm.latest }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney-protobufs" % "{{ git.tag or scm.latest }}"
```

```scala
import io.scalaland.chimney.protobufs._
```

See [Protocol Buffers integration cookbook](/cookbook/#protocol-buffers-integration) for more information.
