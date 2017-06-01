# Chimney

[![Build Status](https://travis-ci.org/scalalandio/chimney.svg?branch=master)](https://travis-ci.org/scalalandio/chimney)
[![Maven Central](https://img.shields.io/maven-central/v/io.scalaland/chimney_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cchimney)
[![codecov.io](http://codecov.io/github/scalalandio/chimney/coverage.svg?branch=master)](http://codecov.io/github/scalalandio/chimney?branch=master)
[![License](http://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)

Scala library for boilerplate free data rewriting

## Adding library to the project

    libraryDependencies += "io.scalaland" %% "chimney" % "0.1.0"

Due to [SI-7046](https://issues.scala-lang.org/browse/SI-7046) some derivations require at least Scala 2.12.1.

## Basic product type rewriting

In basic case we are trying to rewrite one product-type e.g. case class
into another. For simplicity we can assume that respective types match
and corresponding fields have the same names. Then we could transform
them like this:

    import io.scalaland.chimney.dsl._

    case class Catterpillar(size: Int, name: String)
    case class Butterfly(size: Int, name: String)
    val steveTheCatterpillar = Catterpillar(10, "Steve")
    val steveTheButterfly = steveTheCatterpillar.into[Butterfly].transform
    // steveTheButterfly: Butterfly = Butterfly(10,Steve)

In this very basic case we can also use syntax with a single call:

    val steveTheButterfly = steveTheCatterpillar.transformInto[Butterfly]

As a matter of the fact we can not only copy fields by name, when they
exist, but also drop them if target type doesn't need them:

    case class User(id: Long, details: String)
    case class ApiUser(details: String)

    User(1L, "our user").transformInto[ApiUser]
    // ApiUser = ApiUser(our user)

As one might expect, usually we won't have such simple use cases. We
might need to provide some value absent from the base type, or calculate
it from original object:

    case class Student(name: String, education: String)
    case class Employee(name: String, education: String, experience: List[String])

    Student("Paul", "University of Things").into[Employee]
        .withFieldConst('experience, List("Internship in Z Company"))
        .transform
    // Employee = Employee(Paul,University of Things,List(Internship in Z Company))
    Student("Paula", "University of Things").into[Employee]
        .withFieldComputed('experience, student => List(s"${student.name}'s own company"))
        .transform
    // Employee = Employee(Paula,University of Things,List(Paula's own company))

Sometimes a field just change its name:

    case class SpyGB(name: String, surname: String)
    case class SpyRU(imya: String, familia: String)

    SpyGB("James", "Bond").into[SpyRU]
        .withFieldRenamed('name, 'imya)
        .withFieldRenamed('surname, 'familia)
        .transform
    // SpyRU = SpyRU(James,Bond)

Additionally library should out-of-the support mappings for:

  * value classes,
  * basic collections,
  * enumerations.
