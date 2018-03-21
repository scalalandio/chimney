# Chimney

[![Build Status](https://travis-ci.org/scalalandio/chimney.svg?branch=master)](https://travis-ci.org/scalalandio/chimney)
[![Maven Central](https://img.shields.io/maven-central/v/io.scalaland/chimney_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cchimney)
[![codecov.io](http://codecov.io/github/scalalandio/chimney/coverage.svg?branch=master)](http://codecov.io/github/scalalandio/chimney?branch=master)
[![License](http://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)

Scala library for boilerplate-free data transformations.

In the daily life of a strongly-typed language's programmer sometimes happens
to transform an object of one type to another object which contains
a number of the same or similar fields in their definitions.
      
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
Underneath it uses type-level meta-programming based on
[Shapeless](https://github.com/milessabin/shapeless) and type-class
derivation to give you type-safety at compile-time!

```scala
import io.scalaland.chimney.dsl._

val event = command.into[CoffeeMade]
  .withFieldComputed(_.at, _ => ZonedDateTime.now)
  .withFieldRenamed(_.addict, _.forAddict)
  .transform
```

## Getting started

To include Chimney to your SBT project, add following line to you `build.sbt`:

```scala
libraryDependencies += "io.scalaland" %% "chimney" % "0.1.8"
```

Library is released for Scala 2.11 and 2.12.
If you want to use it with Scala.js, you need to replace `%%` with `%%%`.
Due to some [compiler bugs](https://issues.scala-lang.org/browse/SI-7046),
it's recommended to use at least Scala 2.11.9 or 2.12.1.

## Documentation

Chimney documentation is available at https://scalalandio.github.io/chimney
