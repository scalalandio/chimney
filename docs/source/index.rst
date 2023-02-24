Welcome to Chimney's documentation!
===================================

**Chimney** is a Scala library for boilerplate-free data transformations.

Transformers
------------

In the daily life of a strongly-typed language's programmer sometimes it
happens we need to transform an object of one type to another object which
contains a number of the same or similar fields in their definitions.

.. code-block:: scala

  case class MakeCoffee(id: Int, kind: String, addict: String)
  case class CoffeeMade(id: Int, kind: String, forAddict: String, at: ZonedDateTime)

Usual approach is to just rewrite fields one by one

.. code-block:: scala

  val command = MakeCoffee(id = Random.nextInt,
                           kind = "Espresso",
                           addict = "Piotr")
  // MakeCoffee(24, "Espresso", "Piotr")

  val event = CoffeeMade(id = command.id,
                         kind = command.kind,
                         forAddict = command.addict,
                         at = ZonedDateTime.now)
  // CoffeeMade(24, "Espresso", "Piotr", "2020-02-03T20:26:59.659647+07:00[Asia/Bangkok]")


While the example stays short, in real-life code we usually end up with tons
of such boilerplate, especially when:

- we maintain typed schema and want to migrate between multiple schema versions
- we apply practices like DDD (Domain-Driven-Design) where suggested
  approach is to separate model schemas of different bounded contexts
- we use code-generation tools like Protocol Buffers that generate primitive
  types like ``Int`` or ``String``, while you'd prefer to use value objects
  in your domain-level code to improve type-safety and readability

Chimney provides a compact DSL with which you can define transformation
rules and transform your objects with as little boilerplate as possible.

.. code-block:: scala

  import io.scalaland.chimney.dsl._

  val event = command.into[CoffeeMade]
    .withFieldComputed(_.at, _ => ZonedDateTime.now)
    .withFieldRenamed(_.addict, _.forAddict)
    .transform
  // CoffeeMade(24, "Espresso", "Piotr", "2020-02-03T20:26:59.659647+07:00[Asia/Bangkok]")


Read :ref:`partial-transformers/getting-started:Getting started with transformers` to learn more about
Chimney's transformers.

Partial transformers
--------------------

Examples so far described situation when every value of one type can be converted into another type. But what when only
some values can be converted?

.. code-block:: scala

  case class UserForm(name: String, ageInput: String, email: Option[String])
  case class User(name: String, age: Int, email: String)

  UserForm("John", "21", Some("john@example.com")) // ??
  UserForm("Ted", "eighteen", None) // ??

What to do with the rest of them? Return ``null``? Throw ``Exception``? Chimney provides safer
alternative:

.. code-block:: scala

  import io.scalaland.chimney._
  import io.scalaland.chimney.dsl._
  import io.scalaland.chimney.partial._

  val success = UserForm("John", "21", Some("john@example.com")).intoPartial[User]
    .withFieldComputedPartial(_.age, form => Result.fromCatching(form.ageInput.toInt))
    .transform
 val failure = UserForm("Ted", "eighteen", None).intoPartial[User]
    .withFieldComputedPartial(_.age, form => Result.fromCatching(form.ageInput.toInt))
    .transform

Partial transformers allow you to conditionally transform some elements of your data, and if they fail, provide you
with information which element failed and how.

.. code-block:: scala

  success.asOption
  // Some(User("name", 21, "john@example.com"))
  failure.asOption
  // None

  success.asErrorMessageStrings
  // Iterable()
  failure.asErrorMessageStrings
  // Iterable("age" -> "For input string: \"eighteen\"", "email" -> "empty value")

Read :ref:`partial-transformers/partial-transformers:Partial transformers` to learn more about
Chimney's partial transformers.

Patching
--------

Beside transformers, Chimney supports case class patching as well.
It's a bit different type of transformation which happens when you
hold an object of some type and want to modify only subset of its
fields with values taken from other (*patch*) object.

.. code-block:: scala

  case class User(id: Int, email: String, address: String, phone: Long)
  case class UserUpdateForm(email: String, phone: Long)

  val user = User(10, "abc@example.com", "Broadway", 123456789L)
  val updateForm = UserUpdateForm("xyz@example.com", 123123123L)

  user.patchUsing(updateForm)
  // User(10, "xyz@example.com", "Broadway", 123123123L)

Read :ref:`patchers/getting-started:Getting started with patchers` to learn more about
Chimney's patchers.

Chimney's features
------------------

Chimney uses Scala macros to give you:

- type-safety at compile-time
- fast generated code, almost equivalent to hand-written version
- excellent error messages
- minimal overhead on compilation time
- a way to express custom transformation logic

Contents:
=========

.. toctree::
   :maxdepth: 2
   :caption: Getting started

   getting-started/quickstart
   getting-started/community

.. toctree::
   :maxdepth: 2
   :caption: Transformers

   transformers/getting-started
   transformers/customizing-transformers
   transformers/standard-transformers
   transformers/default-values
   transformers/own-transformations
   transformers/scoped-configuration
   transformers/java-beans
   transformers/unsafe-options

.. toctree::
   :maxdepth: 2
   :caption: Partial Transformers

   partial-transformers/partial-transformers
   partial-transformers/migrating-from-lifted
   partial-transformers/total-vs-partial-conflicts
   partial-transformers/cats-integration

.. toctree::
   :maxdepth: 2
   :caption: Lifted Transformers

   lifted-transformers/lifted-transformers
   lifted-transformers/cats-integration

.. toctree::
   :maxdepth: 2
   :caption: Patchers

   patchers/getting-started
   patchers/redundant-fields
   patchers/options-handling

.. toctree::
   :maxdepth: 1
   :caption: Benchmarks

   benchmarks

