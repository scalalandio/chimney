Quickstart
==========

Using SBT
---------

To include Chimney to your SBT project, add the following line
to your ``build.sbt``:

.. parsed-literal::

  libraryDependencies += "io.scalaland" %% "chimney" % "|version|"


Library is released for Scala 2.12.x and 2.13.x. If you want to
use it with Scala.js, you need to replace ``%%`` with ``%%%``.

.. warning:: Due to some `compiler bugs <https://issues.scala-lang.org/browse/SI-7046>`_,
  it's recommended to use at least Scala 2.12.1.

Using Ammonite
--------------

The quickest way to try out Chimney is to use a script that downloads
`coursier <https://github.com/alexarchambault/coursier>`_ and use it
to fetch `Ammonite REPL <https://github.com/lihaoyi/Ammonite>`_ with the
latest version of Chimney. It drops you immediately into a REPL session.

.. code-block:: scala

  curl -s https://raw.githubusercontent.com/scalalandio/chimney/master/try-chimney.sh | bash
  Loading...
  Welcome to the Ammonite Repl 2.0.4 (Scala 2.13.1 Java 1.8.0_192)

  @@ case class Foo(x: String, y: Int)
  defined class Foo

  @@ case class Bar(x: String, y: Int, z: Boolean = true)
  defined class Bar

  @@ Foo("abc", 10).transformInto[Bar]
  res2: Bar = Bar("abc", 10, true)

