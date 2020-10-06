.. _customizing-transformers:

Customizing transformers
========================

Let's add a field to our ``Butterfly`` case class.

.. code-block:: scala

  case class Catterpillar(size: Int, name: String)
  case class Butterfly(size: Int, name: String, wingsColor: String)

Now, when trying to perform the same transformation as
in :ref:`Getting started with transformers`, we get compile-time error.
This is naturally expected, as we don't have any data source for
new ``wingsColor`` field.


.. code-block:: scala

  val stevie = Catterpillar(5, "Steve")
  val steve = stevie.transformInto[Butterfly]
  // error: Chimney can't derive transformation from Catterpillar to Butterfly
  //
  // Butterfly
  //   wingsColor: String - no accessor named wingsColor in source type Catterpillar
  //
  // Consult https://scalalandio.github.io/chimney for usage examples.
  //
  //        val steve = stevie.transformInto[Butterfly]
  //


Providing missing values
------------------------

In this scenario, we can use Chimney's syntax to provide a missing value.
Notice that ``transformInto[T]`` is a shortcut for ``into[T].transform``,
where the latter form allow us to provide additional transformation rules.

With ``withFieldConst`` operation, we can provide exact value for specific field.

.. code-block:: scala

  val steve = stevie.into[Butterfly]
    .withFieldConst(_.wingsColor, "white")
    .transform
  // Butterfly(5, "Steve", "white")

With ``withFieldComputed`` operation we can construct field value dynamically,
by providing a function.

.. code-block:: scala

  val steve = stevie.into[Butterfly]
    .withFieldComputed(_.wingsColor, c => if(c.size > 4) "yellow" else "gray")
    .transform
  // Butterfly(5, "Steve", "yellow")


Fields renaming
---------------

Sometimes a field only change its name. In such case you can
use ``withFieldRenamed`` operation to instruct the library about
performed renaming.

.. code-block:: scala

  case class SpyGB(name: String, surname: String)
  case class SpyRU(imya: String, familia: String)

  val jamesGB = SpyGB("James", "Bond")

  val jamesRU = jamesGB.into[SpyRU]
      .withFieldRenamed(_.name, _.imya)
      .withFieldRenamed(_.surname, _.familia)
      .transform
  // SpyRU("James", "Bond")


Using method accessors
----------------------

By default, Chimney will only consider ``val`` and ``lazy val`` defined within the source type,
because methods may perform side effects (e.g. mutation some state in the source object).

You can ask Chimney to consider methods with ``.enableMethodAccessors``. Note that only methods that are public
and have no parameter list are considered.

.. code-block:: scala

  case class Foo(a: Int) {
    def m: String = "m"
  }
  case class FooV2(a: Int, m: String)

  Foo(1)
    .into[FooV2]
    .enableMethodAccessors
    .transform
  // FooV2(1, "m")


Transforming coproducts
-----------------------

With Chimney you can not only transform case classes, but
sealed trait hierarchies (also known as coproducts) as well.
Consider two following hierarchy definitions.

.. code-block:: scala

  sealed trait Color
  object Color {
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color
  }

  sealed trait Channel
  object Channel {
    case object Alpha extends Channel
    case object Blue extends Channel
    case object Green extends Channel
    case object Red extends Channel
  }

Because of object names correspondence, we can transform ``Color``
to a ``Channel`` in a simple way.

.. code-block:: scala

  val colRed: Color = Color.Red
  val chanRed = colRed.transformInto[Channel]
  // chanRed: Channel = Red

How about other way round?

.. code-block:: scala

  chanRed.transformInto[Color]
  // error: Chimney can't derive transformation from Channel to Color
  //
  // Color
  //   can't transform coproduct instance Channel.Alpha to Color
  //
  // Consult https://scalalandio.github.io/chimney for usage examples.
  //
  //        chanRed.transformInto[Color]
  //                             ^

This time we tried to transform a ``Channel`` to a ``Color``.
Notice that in this case we don't have defined case object in target
hierarchy with corresponding name for ``case object Alpha``.
Wanting to keep the transformation total, we need to somehow provide
a value from a target domain. We can use ``withCoproductInstance`` to
do that. Let's convert any ``Channel.Alpha`` to ``Color.Blue``.

.. code-block:: scala

  val red = chanRed.into[Color]
    .withCoproductInstance { (_: Channel.Alpha.type) => Color.Blue }
    .transform
  // red: Color = Red

  val alpha: Channel = Channel.Alpha
  val blue = alpha.into[Color]
    .withCoproductInstance { (_: Channel.Alpha.type) => Color.Blue }
    .transform
  // blue: Color = Blue


After providing a default, Chimney can prove the transformation
is total and use provided function, when it's needed.

Transformations between flat sealed trait hierarchies and deep trait
hierarchies containing nested sealed traits are also available.
