# Chimney <img src="chimney.png" alt="Chimney logo" width="64" />

![CI build](https://github.com/scalalandio/chimney/workflows/CI%20build/badge.svg)
[![Maven Central](https://img.shields.io/maven-central/v/io.scalaland/chimney_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cchimney)
[![Javadocs](https://www.javadoc.io/badge/io.scalaland/chimney_2.11.svg?color=red&label=scaladoc)](https://www.javadoc.io/doc/io.scalaland/chimney_2.11)
[![codecov.io](http://codecov.io/github/scalalandio/chimney/coverage.svg?branch=master)](http://codecov.io/github/scalalandio/chimney?branch=master)
[![License](http://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt) [![Join the chat at https://gitter.im/scalalandio/chimney](https://badges.gitter.im/scalalandio/chimney.svg)](https://gitter.im/scalalandio/chimney?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.17.svg)](https://www.scala-js.org)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-1.0.0.svg)](https://www.scala-js.org)

Scala library for boilerplate-free data transformations.

In the daily life of a strongly-typed language's programmer sometimes it
happens we need to transform an object of one type to another object which
contains a number of the same or similar fields in their definitions.
      
```scala
case class MakeCoffee(id: Int, kind: String, addict: String)
case class CoffeeMade(id: Int, kind: String, forAddict: String, at: ZonedDateTime)
```
Usual approach is to just rewrite fields one by one
```scala
val command = MakeCoffee(id = Random.nextInt,
                         kind = "Espresso",
                         addict = "Piotr")
val event = CoffeeMade(id = command.id,
                       kind = command.kind,
                       forAddict = command.addict,
                       at = ZonedDateTime.now)
```

While the example stays lean, in real-life code we usually end up with tons
of such boilerplate, especially when:
- we maintain typed schema and want to migrate between multiple schema versions
- we apply practices like DDD (Domain-Driven-Design) where suggested
  approach is to separate model schemas of different bounded contexts
- we use code-generation tools like Protocol Buffers that generate primitive
  types like `Int` or `String`, while you'd prefer to
  use value objects in you domain-level code to improve type-safety
  and readability  


Chimney provides a compact DSL with which you can define transformation
rules and transform your objects with as little boilerplate as possible.

```scala
import io.scalaland.chimney.dsl._

val event = command.into[CoffeeMade]
  .withFieldComputed(_.at, _ => ZonedDateTime.now)
  .withFieldRenamed(_.addict, _.forAddict)
  .transform
```

Underneath it uses Scala macros to give you:
- type-safety at compile-time
- fast generated code, almost equivalent to hand-written version
- excellent error messages
- minimal overhead on compilation time

## Getting started

To include Chimney to your SBT project, add the following line to your `build.sbt`:

```scala
libraryDependencies += "io.scalaland" %% "chimney" % "0.5.3"
```

Library is released for Scala 2.11.x, 2.12.x and 2.13.x.

If you want to use it with Scala.js (or Scala Native), you need to replace `%%` with `%%%`.
Due to some [compiler bugs](https://issues.scala-lang.org/browse/SI-7046),
it's recommended to use at least Scala 2.11.9 or 2.12.1.

### Trying with Ammonite REPL

The quickest way to try out Chimney is to use a script that downloads
[coursier](https://github.com/alexarchambault/coursier) and uses it to fetch
[Ammonite](https://github.com/lihaoyi/Ammonite) REPL with the latest version
of Chimney. It drops you immediately into a REPL session.

```
curl -s https://raw.githubusercontent.com/scalalandio/chimney/master/try-chimney.sh | bash
Loading...
Welcome to the Ammonite Repl 1.1.0
(Scala 2.12.4 Java 1.8.0_152)
If you like Ammonite, please support our development at www.patreon.com/lihaoyi
@ case class Foo(x: String, y: Int) 
defined class Foo

@ case class Bar(x: String, y: Int, z: Boolean = true) 
defined class Bar

@ Foo("abc", 10).transformInto[Bar] 
res2: Bar = Bar("abc", 10, true)
```

## Documentation

Chimney documentation is available at https://scalalandio.github.io/chimney

#### Building documentation locally

In order to build documentation locally, you need to install
[Sphinx](https://www.sphinx-doc.org) documentation generator first.

Then in project's root directory run command:

```
sbt makeSite
```

HTML Documentation should be generated at `target/sphinx/html/index.html`.


## Thanks

Thanks to [JProfiler (Java profiler)](https://www.ej-technologies.com/products/jprofiler/overview.html)
for helping us develop the library and allowing us to use it during development.
