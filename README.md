# Chimney <img src="chimney.png" alt="Chimney logo" width="64" />

[![chimney Scala version support](https://index.scala-lang.org/scalalandio/chimney/chimney/latest.svg)](https://index.scala-lang.org/scalalandio/chimney/chimney)

![CI build](https://github.com/scalalandio/chimney/workflows/CI%20build/badge.svg)
[![codecov.io](http://codecov.io/github/scalalandio/chimney/coverage.svg?branch=master)](http://codecov.io/github/scalalandio/chimney?branch=master)
[![License](http://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt) [![Join the chat at https://gitter.im/scalalandio/chimney](https://badges.gitter.im/scalalandio/chimney.svg)](https://gitter.im/scalalandio/chimney?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Build docs](https://github.com/scalalandio/chimney/workflows/Build%20docs/badge.svg)](https://scalalandio.github.io/chimney/)
[![Scaladoc 2.11](https://javadoc.io/badge2/io.scalaland/chimney_2.11/scaladoc%202.11.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.11)
[![Scaladoc 2.12](https://javadoc.io/badge2/io.scalaland/chimney_2.12/scaladoc%202.12.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.12)
[![Scaladoc 2.13](https://javadoc.io/badge2/io.scalaland/chimney_2.13/scaladoc%202.13.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.13)

Battle tested Scala library for boilerplate-free data transformations.

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

- we keep separate models of different layers in the system
- we apply practices like DDD (Domain-Driven-Design) where suggested
  approach is to separate models of different bounded contexts
- we use code-generation tools like Protocol Buffers that generate primitive
  types like `Int` or `String`, while you'd prefer to use value objects in your
  domain-level code to improve type-safety and readability
- we maintain typed, versioned schemas and want to migrate between multiple schema versions

Chimney provides a compact DSL with which you can define transformation
rules and transform your objects with as little boilerplate as possible.

```scala
import io.scalaland.chimney.dsl._

val event = command.into[CoffeeMade]
  .withFieldComputed(_.at, _ => ZonedDateTime.now)
  .withFieldRenamed(_.addict, _.forAddict)
  .transform
  // CoffeeMade(24, "Espresso", "Piotr", "2020-02-03T20:26:59.659647+07:00[Europe/Warsaw]")
```

For computations that may potentially fail, Chimney provides partial transformers.

```scala
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.partial._

case class UserForm(name: String, ageInput: String, email: Option[String])
case class User(name: String, age: Int, email: String)

UserForm("John", "21", Some("john@example.com"))
  .intoPartial[User]
  .withFieldComputedPartial(_.age, form => Result.fromCatching(form.ageInput.toInt))
  .transform
  .asOption  // Some(User("name", 21, "john@example.com"))

val result = UserForm("Ted", "eighteen", None)
  .intoPartial[User]
  .withFieldComputedPartial(_.age, form => Result.fromCatching(form.ageInput.toInt))
  .transform
  
result.asOption // None
result.asErrorMessageStrings 
// Iterable("age" -> "For input string: \"eighteen\"", "email" -> "empty value")
```

The library uses Scala macros underneath, to give you:
- type-safety at compile-time
- efficient generated code, almost equivalent to hand-written version
- excellent compilation error messages
- minimal overhead on compilation time

## Getting started

To include Chimney to your SBT project, add the following line to your `build.sbt`:

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney" % chimneyVersion
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney" % chimneyVersion
```

where the latest versions available on Maven for each platform are

[![Chimney JVM versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=jvm)](https://search.maven.org/artifact/io.scalaland/chimney_2.13) <br>
[![Chimney Scala.js 1.x versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=sjs1)](https://search.maven.org/artifact/io.scalaland/chimney_sjs1_2.13) <br>
[![Chimney Scala.js 0.6 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=sjs0.6)](https://search.maven.org/artifact/io.scalaland/chimney_sjs0.6_2.13) <br>
[![Chimney Scala Native 0.4 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.4)](https://search.maven.org/artifact/io.scalaland/chimney_native0.4_2.13) <br>
[![Chimney Scala Native 0.3 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.3)](https://search.maven.org/artifact/io.scalaland/chimney_native0.3_2.11) <br>

Library is currently supported for Scala 2.12.x and 2.13.x on JVM, SJS 1.x, SN 0.4. Other versions should be considered EOL.

Due to some [compiler bugs](https://issues.scala-lang.org/browse/SI-7046),
it's recommended to use at least Scala 2.12.1.

### Trying out with Scala CLI/Ammonite

If you are using Scala CLI you can try out Chimney by adding it with `using` clause:
```scala
//> using scala "2.13.10"
//> using dep "io.scalaland::chimney:0.7.2"
import io.scalaland.chimney.dsl._

case class Foo(x: String, y: Int, z: Boolean = true)
case class Bar(x: String, y: Int)

object Main extends App {
  println(Foo("abc", 10).transformInto[Bar])
  println(Bar("abc", 10).into[Foo].enableDefaultValues.transform)
}
```

or run the Ammonite REPL:

```scala
scala-cli --power repl --ammonite --scala "2.13.10" --dependency "io.scalaland::chimney:0.7.2"
Loading...
Welcome to the Ammonite Repl 2.5.6-1-f8bff243 (Scala 2.13.10 Java 17.0.1)
@ case class Foo(x: String, y: Int, z: Boolean = true)
defined class Foo

@ case class Bar(x: String, y: Int)
defined class Bar

@ import io.scalaland.chimney.dsl._
import io.scalaland.chimney.dsl._

@ Foo("abc", 10).transformInto[Bar]
res3: Bar = Bar(x = "abc", y = 10)

@ Bar("abc", 10).into[Foo].enableDefaultValues.transform
res4: Foo = Foo(x = "abc", y = 10, z = true)
```

If you don't have Scala CLI installed you can use this quick script that downloads
[coursier](https://github.com/alexarchambault/coursier) and uses it to fetch
[Ammonite](https://github.com/lihaoyi/Ammonite) REPL with the latest version
of Chimney. It drops you immediately into a REPL session.

```
curl -s https://raw.githubusercontent.com/scalalandio/chimney/master/try-chimney.sh | bash
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

Alternatively use Docker:

```bash
docker run --rm -v "$PWD/docs:/docs" sphinxdoc/sphinx:3.2.1 bash -c "pip install sphinx-rtd-theme && make html"
```

## Thanks

Thanks to [JProfiler (Java profiler)](https://www.ej-technologies.com/products/jprofiler/overview.html)
for helping us develop the library and allowing us to use it during development.

Thanks to [SwissBorg](https://swissborg.com) for sponsoring the development time for this project.
