# Quickstart

[![Chimney JVM versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=jvm)](https://search.maven.org/artifact/io.scalaland/chimney_2.13) <br>
[![Chimney Scala.js 1.x versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=sjs1)](https://search.maven.org/artifact/io.scalaland/chimney_sjs1_2.13) <br>
[![Chimney Scala.js 0.6 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=sjs0.6)](https://search.maven.org/artifact/io.scalaland/chimney_sjs0.6_2.13) <br>
[![Chimney Scala Native 0.4 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.4)](https://search.maven.org/artifact/io.scalaland/chimney_native0.4_2.13) <br>
[![Chimney Scala Native 0.3 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.3)](https://search.maven.org/artifact/io.scalaland/chimney_native0.3_2.11) <br>

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney" % "{{ git.tag }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney" % "{{ git.tag }}"
```

```scala
import io.scalaland.chimney.dsl._
```

If you are advanced used who want to learn about the difference about automatic derivation and semiautomatic derivation
see [Automatic, semiautomatic and inlined derivation](/cookbook/#automatic-semiautomatic-and-inlined-derivation).

## Java collections integration

TODO

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney-java-collections" % "{{ git.tag }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney-java-collections" % "{{ git.tag }}"
```

```scala
import io.scalaland.chimney.javacollections._
```

## Cats integration

TODO

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney-cats" % "{{ git.tag }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney-cats" % "{{ git.tag }}"
```

```scala
import io.scalaland.chimney.cats._
```

## Protocol Buffers integration

TODO

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney-protobufs" % "{{ git.tag }}"
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney-protobufs" % "{{ git.tag }}"
```

```scala
import io.scalaland.chimney.protobufs._
```
