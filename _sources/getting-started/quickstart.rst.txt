Quickstart
==========

Using SBT
---------

To include Chimney to your SBT project, add the following line
to your ``build.sbt``:

.. parsed-literal::

  // if you use Scala on JVM-only
  libraryDependencies += "io.scalaland" %% "chimney" % "|version|"
  // if you cross-compile to Scala.js and/or Scala Native
  libraryDependencies += "io.scalaland" %%% "chimney" % "|version|"


Library is currently supported for Scala 2.12.x and 2.13.x on JVM, ScalaJS 1.x,
Scala Native 0.4.

.. warning:: Due to some `compiler bugs <https://issues.scala-lang.org/browse/SI-7046>`_,
  it's recommended to use at least Scala 2.12.1.

Using Scala CLI/Ammonite
------------------------

If you are using Scala CLI you can try out Chimney by adding it with `using` clause:

.. code-block:: scala

  //> using scala "2.13.10"
  //> using lib "io.scalaland::chimney:0.7.0"
  import io.scalaland.chimney.dsl._

  case class Foo(x: String, y: Int, z: Boolean = true)
  case class Bar(x: String, y: Int)

  object Main extends App {
    println(Foo("abc", 10).transformInto[Bar])
    println(Bar("abc", 10).into[Foo].enableDefaultValues.transform)
  }

or run the Ammonite REPL:

.. code-block:: scala

  scala-cli repl --ammonite --scala "2.13.10" --dependency "io.scalaland::chimney:0.7.0"
  Loading...
  Welcome to the Ammonite Repl 2.5.6-1-f8bff243 (Scala 2.13.10 Java 17.0.1)
  @ case class Foo(x: String, y: Int, z: Boolean = true)
  defined class Foo

  @ case class Bar(x: String, y: Int)
  defined class Bar

  @ import io.scalaland.chimney.dsl._
  import io.scalaland.chimney.dsl._

  @ Foo("abc", 10).transformInto[Bar]
  res3: Bar = Bar(x = "abc", y = 10)

  @ Bar("abc", 10).into[Foo].enableDefaultValues.transform
  res4: Foo = Foo(x = "abc", y = 10, z = true)

If you don't have Scala CLI installed you can use this quick script that downloads
`coursier <https://github.com/alexarchambault/coursier>`_ and use it
to fetch `Ammonite REPL <https://github.com/lihaoyi/Ammonite>`_ with the
latest version of Chimney. It drops you immediately into a REPL session.

.. code-block:: scala

  curl -s https://raw.githubusercontent.com/scalalandio/chimney/master/try-chimney.sh | bash
