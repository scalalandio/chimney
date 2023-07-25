Default values support
======================

Chimney allows case classes' default values as a possible target
field value source.

Enabling default values in generated transformer
------------------------------------------------

.. warning::

  Prior to version 0.7.0 fallback to default values was automatically enabled
  and required explicit disabling.

Field's default value can be enabled as a target value when constructing
target object. The support for them has to be explicitly enabled to avoid
accidents.

.. code-block:: scala

  case class Catterpillar(size: Int, name: String)
  case class Butterfly(size: Int, name: String, wingsColor: String = "purple")

  val stevie = Catterpillar(5, "Steve")

  //val steve = stevie.transformInto[Butterfly] // fails with
  // error: Chimney can't derive transformation from Catterpillar to Butterfly
  //
  // Butterfly
  //   wingsColor: String - no field named wingsColor in source type Catterpillar
  //
  // Consult https://chimney.readthedocs.io for usage examples.
  //
  //            .transform
  //            ^
  val steve = stevie.into[Butterfly].enableDefaultValues.transform
  // Butterfly(5, "Steve", "purple")

Providing the value manually has always a priority over the default.

.. code-block:: scala

  val steve = stevie.into[Butterfly]
    .enableDefaultValues
    .withFieldConst(_.wingsColor, "yellow")
    .transform
  // Butterfly(5, "Steve", "yellow")


Default values for ``Option`` fields
------------------------------------

In case you have added an optional field to a type, wanting to write migration
from old data, usually you set new optional type to ``None``.

.. code-block:: scala

  case class Foo(a: Int, b: String)
  case class FooV2(a: Int, b: String, newField: Option[Double])


Usual approach would be to use ``.withFieldConst`` to set new field value
or give ``newField`` field a default value.

.. code-block:: scala

  Foo(5, "test")
    .into[FooV2]
    .withFieldConst(_.newField, None)
    .transform
  // FooV2(5, "test", None)

At some scale this may turn out to be cumbersome. Therefore, it's possible
to handle such ``Option`` field values for which we can't find counterpart
in source data type as ``None`` by default. You just need to enable
this behavior by using ``.enableOptionDefaultsToNone`` operation.

.. code-block:: scala

  Foo(5, "test")
    .into[FooV2]
    .enableOptionDefaultsToNone
    .transform
  // FooV2(5, "test", None)

Default values for ``Unit`` fields
----------------------------------

Having a target case class type that contains a field of type ``Unit``, Chimney
is able to automatically fill  with unit value (``()``).

.. code-block:: scala

  case class Foo(x: Int, y: String)
  case class Bar(x: Int, y: String, z: Unit)

  Foo(10, "test").transformInto[Bar]
  // Foo(10, test, ())
