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
  in you domain-level code to improve type-safety and readability

Chimney provides a compact DSL with which you can define transformation
rules and transform your objects with as little boilerplate as possible.

.. code-block:: scala

  import io.scalaland.chimney.dsl._

  val event = command.into[CoffeeMade]
    .withFieldComputed(_.at, _ => ZonedDateTime.now)
    .withFieldRenamed(_.addict, _.forAddict)
    .transform
  // CoffeeMade(24, "Espresso", "Piotr", "2020-02-03T20:26:59.659647+07:00[Asia/Bangkok]")


Read :ref:`Getting started with transformers` to learn more about
Chimney's transformers.

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

Read :ref:`Getting started with patchers` to learn more about
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
   transformers/java-beans
   transformers/unsafe-options
   transformers/scoped-configuration
   transformers/lifted-transformers
   transformers/cats-integration


.. toctree::
   :maxdepth: 2
   :caption: Patchers

   patchers/getting-started
   patchers/redundant-fields
   patchers/options-handling

