# Chimney

[![Build Status](https://travis-ci.org/scalalandio/chimney.svg?branch=master)](https://travis-ci.org/scalalandio/chimney)
[![Maven Central](https://img.shields.io/maven-central/v/io.scalaland/chimney_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cchimney)
[![codecov.io](http://codecov.io/github/scalalandio/chimney/coverage.svg?branch=master)](http://codecov.io/github/scalalandio/chimney?branch=master)
[![License](http://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)

Scala library for boilerplate-free data rewriting.

Motivation for it was a annoyance coming from rewriting one case class
into another: once case of such was a need to separate external API from
internal model, the other was process of manually applying migrations to
e.g. some read model:

```scala
case class DomainUser(id: Long, name: String, surname: String, ...)
case class ApiUser(name: String, surname: String, ...)

val domainUser: DomainUser = ...
val apiUser = ApiUser(name = domainUser.name, surname = domainUser.surname, ...)
```

```scala
object version1 {
   case class Transaction(date: LocalDate, description: String, ...)
}
object version2 {
   case class Transaction(date: LocalDate, description: String, ...)
}
val version1Transaction: version1.Transaction = ...
val version2Transaction = version2.Transaction(
   date = version1Transaction.date,
   description = version1Transaction.description,
   ...
)
```

Chimney was created to remove the pain coming from such boilerplate.

## Adding library to the project

```scala
libraryDependencies += "io.scalaland" %% "chimney" % "0.1.5"
```

Due to [SI-7046](https://issues.scala-lang.org/browse/SI-7046) some derivations require at least Scala 2.12.1 or 2.11.9.

## Basic product type rewriting

In basic case we are trying to rewrite one product-type e.g. case class
into another. For simplicity we can assume that respective types match
and corresponding fields have the same names. Then we could transform
them like this:

```scala
import io.scalaland.chimney.dsl._

case class Catterpillar(size: Int, name: String)
case class Butterfly(size: Int, name: String)
val steveTheCatterpillar = Catterpillar(10, "Steve")
val steveTheButterfly = steveTheCatterpillar.into[Butterfly].transform
// steveTheButterfly: Butterfly = Butterfly(10,Steve)
```

In this very basic case we can also use syntax with a single call:

```scala
val steveTheButterfly = steveTheCatterpillar.transformInto[Butterfly]
```

As a matter of the fact we can not only copy fields by name, when they
exist, but also drop them if target type doesn't need them:

```scala
case class User(id: Long, details: String)
case class ApiUser(details: String)

User(1L, "our user").transformInto[ApiUser]
// ApiUser = ApiUser(our user)
```

As one might expect, usually we won't have such simple use cases. We
might need to provide some value absent from the base type, or calculate
it from original object:

```scala
case class Student(name: String, education: String)
case class Employee(name: String, education: String, experience: List[String])

Student("Paul", "University of Things").into[Employee]
    .withFieldConst(_.experience, List("Internship in Z Company"))
    .transform
// Employee = Employee(Paul,University of Things,List(Internship in Z Company))
Student("Paula", "University of Things").into[Employee]
    .withFieldComputed(_.experience, student => List(s"${student.name}'s own company"))
    .transform
// Employee = Employee(Paula,University of Things,List(Paula's own company))
```

Sometimes a field just change its name:

```scala
case class SpyGB(name: String, surname: String)
case class SpyRU(imya: String, familia: String)

SpyGB("James", "Bond").into[SpyRU]
    .withFieldRenamed(_.name, _.imya)
    .withFieldRenamed(_.surname, _.familia)
    .transform
// SpyRU = SpyRU(James,Bond)
```

Additionally library should out-of-the-box support mappings for:

  * value classes,
  * basic collections,
  * enumerations.
