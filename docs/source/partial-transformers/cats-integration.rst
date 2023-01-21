.. _partial-cats-integration:

Cats integration
================

Chimney provides `Cats <https://typelevel.org/cats>`_ library integration module.
To include it to your SBT project, add the following line to your ``build.sbt``:

.. parsed-literal::

  libraryDependencies += "io.scalaland" %% "chimney-cats" % "|version|"

The module is released for Scala 2.12.x and 2.13.x and cats 2.2.0.
If you want to use it with Scala.js, you need to replace ``%%`` with ``%%%``.

The module provides package ``io.scalaland.chimney.cats`` with all the goodies
described here.

.. important::

  You need to import ``io.scalaland.chimney.cats._`` in order to support
  the ``Validated`` and ``Ior`` datatypes for lifted transformers.
