Known issues and limitations
============================

Recursive types fail to compile
-------------------------------

Chimney attempts to avoid unnecessary memory allocations for good performance.

It means that the code ``foo.into[Bar].transform`` would try to avoid creation of
``Transformer[Foo, Bar]`` - if user provided one it would have to be used, but if
the only available would come from automatic derivation, it would be ignored so
that macro would generate an inlined expression.

This isn't possible with recursive types, as you cannot inline potentially unbounded
nesting of transformations. For them it is suggested to derive the ``Transformer``,
assigning it to ``implicit val``/``implicit def`` so that recursive transformations would
be handled by recursive calls. This can be done with:

.. code-block:: scala

  implicit val foo2bar: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]

  // or

  implicit val foo2bar: Transformer[Foo, Bar] = Transformer.define[Foo, Bar].buildTransformer

and then

.. code-block:: scala

  foo.transformInto[Bar] // uses implicit Transformer (with recursive transformation)

The same is true for partial transformers.

Sealed traits fail to compile
-----------------------------

In case of incremental compilation, Zinc compiler sometimes has issues with
caching certain kind of information and macros don't get a proper information
from ``knownDirectSubclasses`` method. It usually helps when you ``clean``
and ``compile`` again. It cannot be fixed in the library as it relies on
the compiler to provide it with this data, and compiler fails to do so.

On Scala 2.12.0 it failed `in other cases as well <https://github.com/scala/bug/issues/7046>`_
so it is recommended to update 2.12 to at least 2.12.1.

Patchers are flat
-----------------

Currently ``Patcher``\s support only flat updates. They cannot perform recursive
update of a class.
