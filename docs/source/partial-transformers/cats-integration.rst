.. _partial-cats-integration:

Cats integration
================

Chimney provides `Cats <https://typelevel.org/cats>`_ library integration module
for partial transformers.
To include it to your SBT project, add the following line to your ``build.sbt``:

.. parsed-literal::

  libraryDependencies += "io.scalaland" %% "chimney-cats" % "|version|"

.. TODO: verify cats version

The module is released for Scala 2.12.x and 2.13.x and cats 2.x.
If you want to use it with Scala.js, you need to replace ``%%`` with ``%%%``.

The module provides package ``io.scalaland.chimney.cats`` with all the goodies
described here.

.. important::

  You need to import ``io.scalaland.chimney.cats._`` in order to support
  the ``Validated`` for lifted transformers.


Contents
--------

Cats integration module contains the following stuff:

* type classes instances for partial transformers data structures

    * ``Applicative`` instance for ``partial.Result``
    * ``Semigroup`` instance for ``partial.Result.Errors``

* integration with ``Validated`` data type

