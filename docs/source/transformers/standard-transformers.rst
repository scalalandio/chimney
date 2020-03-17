.. _standard-transformers:

Standard transformers
=====================

Chimney provides several out of the box transformer generation
rules, which can be applied in any nested context.


Identity transformation
-----------------------

Given any type ``T``, Chimney is able to derive identity
transformer: ``Transformer[T, T]``.

.. code-block:: scala

  1234.transformInto[Int] // 1234: Int
  true.transformInto[Bool] // true: Bool
  3.14159.transformInto[Double] // 3.14159: Double
  "test".transformInto[String] // test: String
  Butterfly(3, "Steve").transformInto[Butterfly] // Butterfly(3, Steve): Butterfly

Supertype transformation
------------------------

Given any types ``T`` and ``U`` such that ``T <: U``
(``T`` is subtype of ``U``), Chimney is able to derive supertype
transformer: ``Transformer[T, U]``.

.. code-block:: scala

  class Vehicle(val maxSpeed: Double)
  class Car(maxSpeed: Double, val seats: Int) extends Vehicle(maxSpeed)

  (new Car(180, 5)).transformInto[Vehicle]
  // Vehicle(180.0)

Value classes
-------------

As nowadays value classes tends to be relatively widely pervasive, Chimney handles
them in a special way, supporting automatic value class field extraction and wrapping.

.. code-block:: scala

  object rich {
    case class PersonId(id: Int) extends AnyVal
    case class PersonName(name: String) extends AnyVal
    case class Person(personId: PersonId, personName: PersonName, age: Int)
  }
  object plain {
    case class Person(personId: Int, personName: String, age: Int)
  }

  val richPerson = rich.Person(PersonId(10), PersonName("Bill"), 30)
  val plainPerson = richPerson.transformInto[plain.Person]
  // plain.Person(10, "Bill", 30)
  val richPerson2 = plainPerson.transformInto[rich.Person]
  // rich.Person(PersonId(10), PersonName("Bill"), 30)


Options
-------

Given any types ``T``, ``U`` such that there exists Chimney
transformer between them (``Transformer[T, U]``), Chimney is able
to derive ``Transformer[Option[T], Option[U]]``.

.. code-block:: scala

  Some(1234).transformInto[Option[Int]]
  // Some(1234): Option[Int]
  Option.empty[Int].transformInto[Option[Int]]
  // None: Option[Int]

  Some("test").transformInto[Option[String]]
  // Some(test): Option[String]
  Option.empty[String].transformInto[Option[String]]
  // None: Option[String]

  Some(new Car(180, 5)).transformInto[Option[Vehicle])
  // Some(Vehicle(180.0)): Option[Vehicle]
  Option.empty[Car].transformInto[Option[Vehicle])
  // None: Option[Vehicle]

  Some(rich.Person(PersonId(10), PersonName("Bill"), 30)).transformInto[Option[plain.Person])
  // Some(plain.Person(10, "Bill", 30)): Option[plain.Person]
  Option.empty[rich.Person].transformInto[Option[plain.Person])
  // None: Option[plain.Person]


Collections
-----------

Given any collection types ``C1[_]`` and ``C2[_]``, and types ``T``, ``U``
such that there exists Chimney transformer between them (``Transformer[T, U]``),
Chimney is able to derive ``Transformer[C1[T], C2[U]]``.

.. code-block:: scala

  List(123, 456).transformInto[Array[Int]]
  // Array(123, 456)

  Seq("foo", "bar").transformInto[Vector[String]]
  // Vector(foo, bar)

  Vector(new Car(160, 4), new Car(220, 5)).transformInto[List[Vehicle]]
  // List(Vehicle(160), Vehicle(220))

Note that ``C1``, ``C2`` may be different collection types like ``List``, ``Vector``,
``Seq``, ``Array``, etc.

Maps
----

Given any collection types ``K1``, ``K2``, ``V1``, ``V2`` such that there
exist transformers ``Transformer[K1, K2]`` and ``Transformer[V1, V2]``,
Chimney is able to derive ``Transformer[Map[K1, V1], Map[K2, V2]]``.

.. code-block:: scala

  Map(1 -> "Alice", 2 -> "Bob").transformInto[Map[Int, PersonName]]
  // Map(1 -> PersonName(Alice), 2 -> PersonName(Bob))

  Map(PersonId(10) -> new Car(200, 5), PersonId(22) -> new Car(170, 4)).transformInto[Map[Int, Vehicle]]
  // Map(10 -> Vehicle(200), 22 -> Vehicle(170))


Either
------

Given any collection types ``L1``, ``L2``, ``R1``, ``R2`` such that there
exist transformers ``Transformer[L1, L2]`` and ``Transformer[R1, R2]``,
Chimney is able to derive ``Transformer[Either[L1, R1], Either[L2, R2]]``.

.. code-block:: scala

  (Right("Batman"): Either[Int, String]).transformInto[Either[PersonId, PersonName]]
  // Right(PersonName(Batman)): Either[PersonId, PersonName]

  (Left(10): Either[Int, String]).transformInto[Either[PersonId, PersonName]]
  // Left(PersonId(10)): Either[PersonId, PersonName]

  (Right(Array(10, 20)): Either[String, Array[Int]]).transformInto[Either[String, List[Int]]]
  // Right(List(10, 20)): Either[String, List[Int]]

  (Left("test"): Either[String, Array[Int]]).transformInto[Either[String, List[Int]]]
  // Left(test): Either[String, List[Int]]
