package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.PatchDomain.*

import scala.annotation.nowarn

/** Verifies that patcher DSL chains ([[dsl.PatcherDefinition]] / [[dsl.PatcherUsing]]) with various combinations of
  * data-carrying and type-only modifiers produce correct results — both from fresh expressions and from `val` references
  * (branching).
  */
@nowarn("msg=unused import")
class PatcherAllocationsSpec extends ChimneySpec {

  group("PatcherDefinition fresh expression chains") {

    test("0 data-carrying modifiers (empty chain)") {
      val p = Patcher.define[User, UpdateDetails].buildPatcher
      p.patch(exampleUser, UpdateDetails("new@email.com", 9876543210L)) ==>
        User(10, Email("new@email.com"), Phone(9876543210L))
    }

    test("1 data-carrying modifier (withFieldConst)") {
      val p = Patcher
        .define[User, UpdateDetails]
        .withFieldConst(_.id, 99)
        .buildPatcher
      p.patch(exampleUser, UpdateDetails("new@email.com", 9876543210L)) ==>
        User(99, Email("new@email.com"), Phone(9876543210L))
    }

    test("multiple data-carrying modifiers (withFieldConst + withFieldComputed)") {
      val p = Patcher
        .define[User, UpdateDetails]
        .withFieldConst(_.id, 99)
        .withFieldComputed(_.id, _ => 77)
        .buildPatcher
      p.patch(exampleUser, UpdateDetails("new@email.com", 9876543210L)) ==>
        User(77, Email("new@email.com"), Phone(9876543210L))
    }

    test("data-carrying + type-only interleaved") {
      val p = Patcher
        .define[User, UpdateDetails]
        .withFieldConst(_.id, 42)
        .enableMacrosLogging
        .disableMacrosLogging
        .withFieldComputed(_.id, _ => 55)
        .buildPatcher
      p.patch(exampleUser, UpdateDetails("x@y.com", 100L)) ==>
        User(55, Email("x@y.com"), Phone(100L))
    }

    test("only type-only modifiers") {
      val p = Patcher
        .define[User, UpdateDetails]
        .enableMacrosLogging
        .disableMacrosLogging
        .buildPatcher
      p.patch(exampleUser, UpdateDetails("new@email.com", 5555L)) ==>
        User(10, Email("new@email.com"), Phone(5555L))
    }

    test("three consecutive data-carrying modifiers") {
      val p = Patcher
        .define[User, UpdateDetails]
        .withFieldConst(_.id, 1)
        .withFieldComputed(_.id, _ => 2)
        .withFieldConst(_.id, 3)
        .buildPatcher
      p.patch(exampleUser, UpdateDetails("ignored", 999L)) ==>
        User(3, Email("ignored"), Phone(999L))
    }
  }

  group("PatcherDefinition val reference chains (branching)") {

    test("shared base with 0 data modifiers, then branching") {
      val base = Patcher.define[User, UpdateDetails]
      val p1 = base.withFieldConst(_.id, 1).buildPatcher
      val p2 = base.withFieldConst(_.id, 2).buildPatcher
      p1.patch(exampleUser, UpdateDetails("a@b.com", 111L)) ==> User(1, Email("a@b.com"), Phone(111L))
      p2.patch(exampleUser, UpdateDetails("a@b.com", 111L)) ==> User(2, Email("a@b.com"), Phone(111L))
    }

    test("shared base with data, then branching with more data") {
      val base = Patcher.define[User, UpdateDetails].withFieldConst(_.id, 0)
      val p1 = base.withFieldComputed(_.id, _ => 1).buildPatcher
      val p2 = base.withFieldComputed(_.id, _ => 2).buildPatcher
      p1.patch(exampleUser, UpdateDetails("x", 1L)) ==> User(1, Email("x"), Phone(1L))
      p2.patch(exampleUser, UpdateDetails("x", 1L)) ==> User(2, Email("x"), Phone(1L))
    }
  }

  group("PatcherUsing (.using) fresh expression chains") {

    test("0 data-carrying modifiers") {
      exampleUser.using(UpdateDetails("new@email.com", 9876543210L)).patch ==>
        User(10, Email("new@email.com"), Phone(9876543210L))
    }

    test("1 data-carrying modifier (withFieldConst)") {
      exampleUser.using(UpdateDetails("x@y.com", 111L)).withFieldConst(_.id, 99).patch ==>
        User(99, Email("x@y.com"), Phone(111L))
    }

    test("multiple data-carrying modifiers") {
      exampleUser
        .using(UpdateDetails("x@y.com", 111L))
        .withFieldConst(_.id, 99)
        .withFieldComputed(_.id, _ => 77)
        .patch ==> User(77, Email("x@y.com"), Phone(111L))
    }

    test("data-carrying + type-only interleaved") {
      exampleUser
        .using(UpdateDetails("x@y.com", 111L))
        .withFieldConst(_.id, 42)
        .enableMacrosLogging
        .disableMacrosLogging
        .withFieldComputed(_.id, _ => 55)
        .patch ==> User(55, Email("x@y.com"), Phone(111L))
    }
  }

  group("PatcherUsing val reference chains (branching)") {

    test("shared base, then branching") {
      val base = exampleUser.using(UpdateDetails("x@y.com", 111L))
      val r1 = base.withFieldConst(_.id, 1).patch
      val r2 = base.withFieldConst(_.id, 2).patch
      r1 ==> User(1, Email("x@y.com"), Phone(111L))
      r2 ==> User(2, Email("x@y.com"), Phone(111L))
    }

    test("shared base with data, then branching") {
      val base = exampleUser.using(UpdateDetails("x@y.com", 111L)).withFieldConst(_.id, 0)
      val r1 = base.withFieldComputed(_.id, _ => 1).patch
      val r2 = base.withFieldComputed(_.id, _ => 2).patch
      r1 ==> User(1, Email("x@y.com"), Phone(111L))
      r2 ==> User(2, Email("x@y.com"), Phone(111L))
    }
  }
}
