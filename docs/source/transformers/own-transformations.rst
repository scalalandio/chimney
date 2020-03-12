Plugging in own transformations
===============================

In case the transformation is relatively complex or if for
some reason you just want to bypass Chimney derivation mechanism,
you can always fall back to a simple function that you can plug
into the Chimney transformation.


.. _transformer-typeclass:

``Transformer`` type class
--------------------------

The library defines a type class ``Transformer`` that just
wraps transformation function.

.. code-block:: scala

  trait Transformer[From, To] {
    def transform(src: From): To
  }


You can plug your own transformer in by providing implicit
instance in a local context.

.. code-block:: scala

  import io.scalaland.chimney.dsl._
  import io.scalaland.chimney.Transformer

  object v1 {
    case class User(id: Int, name: String, street: String, postalCode: String)
  }
  object v2 {
    case class Address(street: String, postalCode: String)
    case class User(id: Int, name: String, addresses: List[Address])
  }

  implicit val userV1toV2: Transformer[v1.User, v2.User] =
    (user: v1.User) => v2.User(
      id = user.id,
      name = user.name,
      addresses = List(v2.Address(user.street, user.postalCode))
    )

  val v1Users = List(
    v1.User(1, "Steve", "Love street", "27000"),
    v1.User(2, "Anna", "Broadway", "00321")
  )

  val v2Users = v1Users.transformInto[List[v2.User]]
  // List(
  //   v2.User(1, "Steve", List(Address("Love street", "27000"))),
  //   v2.User(2, "Anna", List(Address("Broadway", "00321")))
  // )

As we can see, Chimney correctly picked and used implicit
``Transformer[v1.User, v2.User]`` defined locally in transformation
between list of users.

But is it really a necessity to define custom transformer
completely manually?


.. _transformer-definition-dsl:

Transformer definition DSL
--------------------------

One can think that if we only need to provide function implementation
of type ``v1.User => v2.User``, why not use Chimney's DSL in order
to generate the transformation?

.. code-block:: scala

  implicit val userV2toV2: Transformer[v1.User, v2.User] =
    (user: v1.User) => user
      .into[v2.User]
      .withFieldComputed(_.addresses, u => List(v2.Address(u.street, u.postalCode)))
      .transform

.. warning:: While it looks reasonably, it will not work as expected :(

Chimney's macro, before trying to derive any transformer, tries to
find instance of required transformer in implicit scope. Unfortunately,
it will pick ``userV2toV2``, because types match, this value is
marked as implicit and is available in macro expansion scope. Depending
on few details, it will either end up as compilation error, or
will lead to ``StackOverflowError`` at runtime.

.. note:: Since version 0.4.0 there is a simple solution to this problem.

We need to use special syntax ``Transformer.define[From, To]``
which introduces us to new transformer builder between types
``From`` and ``To``.

.. code-block:: scala

  implicit val userV2toV2: Transformer[v1.User, v2.User] =
    Transformer.define[v1.User, v2.User]
      .withFieldComputed(_.addresses, u => List(v2.Address(u.street, u.postalCode)))
      .buildTransformer

In transformer builder we can use all operations available
to usual transformer DSL. The only difference is that we don't
call ``.transform`` at the end (since we don't transform value
in place), but ``buildTransformer`` (because we generate
transformer here). Such generated transformer is semantically
equivalent to hand-written transformer from previous section.

Chimney solves self reference implicit problem by not looking
for implicit instance for ``Transformer[From, To]`` when
using transformer builder  ``Transformer.define[From, To]``.

Recursive data types support
----------------------------

Chimney can generate transformers between recursive data structures.
Consider following example.

.. code-block:: scala

  case class Foo(x: Option[Foo])
  case class Bar(x: Option[Bar])


We would like to define transformer instance which would be able
to convert a value ``Foo(Some(Foo(None)))`` to ``Bar(Some(Bar(None)))``.
In order to avoid aforementioned issues with self-referencing, you
must define your recursive transformer instance as ``implicit def``
or ``implicit lazy val``.

.. code-block:: scala

  implicit def fooToBarTransformer: Transformer[Foo, Bar] =
    Transformer.derive[Foo, Bar] // or Transformer.define[Foo, Bar].buildTransformer

  Foo(Some(Foo(None))).transformInto[Bar]
  // Bar(Some(Bar(None)))

