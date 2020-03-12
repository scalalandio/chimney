Getting started with transformers
=================================

In this section you will learn how to use Chimney transformers
example by example.

Basic transformations
---------------------

When target object contains only fields present in the source object,
with corresponding types, we can use shorthanded ``transformInto``.

.. code-block:: scala

  case class Catterpillar(size: Int, name: String)
  case class Butterfly(size: Int, name: String)

  val stevie = Catterpillar(5, "Steve")
  val steve = stevie.transformInto[Butterfly]
  // Butterfly(5, "Steve")

Nested transformations
----------------------

It also works when transformation needs to be recursive, possibly
involving traversal on nested collection.

.. code-block:: scala

  case class Youngs(insects: List[Catterpillar])
  case class Adults(insects: List[Butterfly])

  val kindergarden = Youngs(List(Catterpillar(5, "Steve"), Catterpillar(4, "Joe")))
  val highschool = kindergarden.transformInto[Adults]
  // Adults(List(Butterfly(5, "Steve"), Butterfly(4, "Joe"))

We can use it as long as Chimney can recursively construct transformation
for all fields of a target object. In this example transformer for
``List`` type is constructed basing on automatically derived
``Catterpillar -> Butterfly`` mapping.
