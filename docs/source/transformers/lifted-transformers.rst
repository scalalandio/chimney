.. _lifted-transformers:

Lifted transformers
===================

.. warning::

  This feature is deprecated and most likely will be removed soon.
  Consider using :ref:`partial-transformers` instead.


While Chimney transformers wrap total functions of type ``From => To``, they don't
really support partial transformations, where depending on the input value, transformation
may `succeed` or `fail`.

Let's take a look at the following example.

.. code-block:: scala

  case class RegistrationForm(email: String,
                              username: String,
                              password: String,
                              age: String)

  case class RegisteredUser(email: String,
                            username: String,
                            passwordHash: String,
                            age: Int)

We get field ``age: String`` as an input, but we would like to parse it into correct ``Int``
or signal an error, if provided value is not valid integer. This is simply not possible
with total ``Transformer``. This is a moment when `lifted transformers`, provided
by :ref:`transformerf-type-class` come into play.

.. code-block:: scala

  val okForm = RegistrationForm("john@example.com", "John", "s3cr3t", "40")

  okForm
    .intoF[Option, RegisteredUser] // (1)
    .withFieldComputed(_.passwordHash, form => hashpw(form.password))
    .withFieldComputedF(_.age, _.age.toIntOption) // (2)
    .transform // (3)
  // Some(RegisteredUser("john@example.com", "John", "...", 40)): Option[RegisteredUser]

There are few differences to total transformers in the example above:

1. Instead of ``into[RegisteredUser]``, we use ``intoF[Option, RegisteredUser]``, which
   tells Chimney that ``Option`` type will be used for handling partial transformations.
2. Instead of ``withFieldComputed``, we use ``withFieldComputedF``, where second parameter
   is a function that wraps result into a type constructor provided in `(1)` - ``Option``
   in this case.
3. Result type of ``transform`` call is not ``RegisteredUser``, but ``Option[RegisteredUser]``.

As you expect, when provided ``age`` which is not valid integer, this code evaluates to ``None``.

.. code-block:: scala

  val badForm = RegistrationForm("john@example.com", "John", "s3cr3t", "not an int")

  badForm
    .intoF[Option, RegisteredUser]
    .withFieldComputed(_.passwordHash, form => hashpw(form.password))
    .withFieldComputedF(_.age, _.age.toIntOption)
    .transform
  // None: Option[RegisteredUser]


Lifted DSL operations
---------------------

Similar to ``withFieldConst``, ``withFieldComputed``, ``withCoproductInstance`` operations in DSL,
there are lifted counterparts available:

- ``withFieldConstF``
- ``withFieldComputed``
- ``withCoproductInstanceF``

Analogously to :ref:`transformer-definition-dsl` for ``Transformer``, we can define above transformation
as implicit ``TransformerF[Option, RegistrationForm, RegisteredUser]``. In order to do this,
we use ``TransformerF.define`` (or equivalently ``Transformer.defineF``).

.. code-block:: scala

  implicit val transformer: TransformerF[Option, RegistrationForm, RegisteredUser] =
    TransformerF.define[Option, RegistrationForm, RegisteredUser]
      .withFieldComputed(_.passwordHash, form => hashpw(form.password))
      .withFieldComputedF(_.age, _.age.toIntOption)
      .buildTransformer

As commonly, as with total transformers, this instance may be later picked up and used other,
lifted transformations. In the following example it's used for transforming array of registration
forms into list of registered users.

.. code-block:: scala

  Array(okForm, badForm).transformIntoF[Option, List[RegisteredUser]]
  // None: Option[List[RegisteredUser]]

Note that following error handling semantics for collections, we've got ``None`` as a result
(because not all of array elements were valid forms, according to the defined lifted transformer).

.. _capturing-validation-errors:

Capturing validation errors
---------------------------

Usually, when partial transformation failed, we would like to know `why` it failed.
Thus, we must use different wrapper type than ``Option`` that allows to capture error information.

Chimney supports out of the box ``Either[C[E], +*]``, as the wrapper type, where

- ``E`` - type of a single error occurrence
- ``C[_]`` - collection type to store all the transformation errors (like ``Seq``, ``Vector``, ``List``, etc.)

If we pick error type as ``String`` (as validation error message) and collection as ``Vector``,
we obtain wrapper type ``Either[Vector[String], +*]``.

.. note::

  Type syntax with ``+*`` is only available with
  `kind-projector compiler plugin <https://github.com/typelevel/kind-projector>`_.
  If you don't want to (or can't) use it, you may either use type-lambda with weird syntax:

  .. code-block:: scala

    ({type L[+X] = Either[Vector[String], X]})#L

  or define type alias:

  .. code-block:: scala

    type EitherVecStr[+X] = Either[Vector[String], X]

  and use type ``EitherVecStr`` as a lifted wrapper type.


Let's enhance our ``RegistrationForm`` to ``RegisteredUser`` lifted transformer with few
additional validation rules:

- ``email`` field should contain ``@`` character
- ``age`` must be at least ``18`` years


.. code-block:: scala

  implicit val transformer: TransformerF[EitherVecStr, RegistrationForm, RegisteredUser] = {
    Transformer.defineF[EitherVecStr, RegistrationForm, RegisteredUser]
      .withFieldComputedF(_.email, form => {
        if(form.email.contains('@')) {
          Right(form.email)
        } else {
          Left(Vector(s"${form.username}'s email: does not contain '@' character"))
        }
      })
      .withFieldComputed(_.passwordHash, form => hashpw(form.password))
      .withFieldComputedF(_.age, form => form.age.toIntOption match {
        case Some(value) if value >= 18 => Right(value)
        case Some(value) => Left(Vector(s"${form.username}'s age: must have at least 18 years"))
        case None => Left(Vector(s"${form.username}'s age: invalid number"))
      })
      .buildTransformer
  }

Then, trying to transform multiple registration forms, we can validate all them at once:

.. code-block:: scala

  Array(
    RegistrationForm("john_example.com", "John", "s3cr3t", "10"),
    RegistrationForm("alice@example.com", "Alice", "s3cr3t", "19"),
    RegistrationForm("bob@example.com", "Bob", "s3cr3t", "21.5")
  ).transformIntoF[EitherVecStr, List[RegisteredUser]]
  // Left(
  //   Vector(
  //     "John's email: does not contain '@' character",
  //     "John's age: must have at least 18 years",
  //     "Bob's age: invalid number",
  //   )
  // )

In case when all the provided forms are correct, we obtain requested collection of
registered users, wrapped in ``Right``.

.. code-block:: scala

  Array(
    RegistrationForm("john@example.com", "John", "s3cr3t", "40"),
    RegistrationForm("alice@example.com", "Alice", "s3cr3t", "19"),
    RegistrationForm("bob@example.com", "Bob", "s3cr3t", "21")
  ).transformIntoF[EitherVecStr, List[RegisteredUser]]
  // Right(
  //   List(
  //     RegisteredUser("john@example.com", "John", "...", 40)
  //     RegisteredUser("alice@example.com", "Alice", "...", 19),
  //     RegisteredUser("bob@example.com", "Bob", "...", 21)
  //   )
  // )

.. warning::

   Note that collection type where you gather errors is independent of
   any eventual collection types that takes part in the transformation.

   For ``Either`` wrappers, Chimney supports practically any Scala standard collection
   type, but depending on your choice, you may obtain different performance characteristics.
   Thus, collections with reasonably fast concatenation should be preferred on the
   error channel.


If you prefer to use `Cats <https://typelevel.org/cats>`_ library, you might be
interested in :ref:`cats-validated`.


.. _transformerf-type-class:

``TransformerF`` type class
---------------------------

Similar to the :ref:`transformer-typeclass`, Chimney defines a ``TransformerF`` type class,
which allows to express partial (`lifted`, `wrapped`) transformation of type ``From => F[To]``.

.. code-block:: scala

  trait TransformerF[F[+_], From, To] {
    def transform(src: From): F[To]
  }


The whole library functionality that refers to total transformers,
is also supported for lifted transformers. This especially means:

- local implicit instances of ``TransformerF`` are preferred in the first place,
  before deriving as instance by a macro (read more about it in :ref:`deriving-transformerf`)
- all the ``enable``/``disable`` flags are respected by lifted transformers
- you can customize lifted transformers using any operation described in
  :ref:`customizing-transformers` which works as well for total transformers,
  as for lifted ones
- all the :ref:`standard-transformers` rules are provided for lifted transformers too
- derivation for case classes, tuples, Java beans are supported too

.. note::

  Note that for convenience of some operations, ``F`` is defined with as
  `covariant` type constructor.


Supporting custom ``F[_]``
--------------------------

Chimney provides pluggable interface that allows you to use your own
``F[_]`` type constructor in lifted transformations.

The library defines ``TransformerFSupport`` type class, as follows.

.. code-block:: scala

  trait TransformerFSupport[F[+_]] {
    def pure[A](value: A): F[A]
    def product[A, B](fa: F[A], fb: => F[B]): F[(A, B)]
    def map[A, B](fa: F[A], f: A => B): F[B]
    def traverse[M, A, B](it: Iterator[A], f: A => F[B])(implicit fac: Factory[B, M]): F[M]
  }

.. important::

  Chimney macros, during lifted transformer derivation, resolve implicit instance
  of ``TransformerFSupport`` for requested wrapper type constructor and use it
  in various places in emitted code.

In order to be able to use wrapper type of your choice, you need to implement
an instance of ``TransformerFSupport`` and put it as implicit term in the scope of usage.

For those familiar with `applicative functors` and `traversable` type classes,
implementation of these methods should be obvious. Yet it gives some choice about
semantics of error handling.

Chimney supports ``Option``, ``Either`` and ``cats.data.Validated``
(in :ref:`cats-integration`) just exactly by providing implicit instaces of
``TransformerFSupport`` implemented for those wrapper types.


Error path support
--------------------------

.. warning::

    Support for enhanced error paths is currently an experimental feature and we don't
    guarantee it will be included in the next library versions in the same shape.

Chimney provides ability to trace errors in lifted transformers.
For using it you need to implement an instance of ``TransformerFErrorPathSupport``

.. code-block:: scala

    trait TransformerFErrorPathSupport[F[+_]] {
      def addPath[A](fa: F[A], node: ErrorPathNode): F[A]
    }

There are 4 different types of of ``ErrorPathNode``:
    - ``Accessor`` for case class field or java bean getter
    - ``Index`` for collection index
    - ``MapKey`` for map key
    - ``MapValue`` for map value

In case if Chimney can resolve instance of ``TransformerFErrorPathSupport`` in scope of your
lifted transformer, each error in transformation will contain path of nodes to error location

Out of box Chimney contains instance for Either[C[TransformationError[M]], +*], where
    - ``M`` - type of error message
    - ``C[_]`` - collection type to store all the transformation errors (like Seq, Vector, List, etc.)
    - ``TransformationError`` - default implementation of error containing path

Let’s take a look at the following example:

.. code-block:: scala

    type V[+A] = Either[List[TransformationError[String]], A]

    implicit val intParse: TransformerF[V, String, Int] =
      str => Try(str.toInt).toEither.left.map(_ => List(TransformationError(s"Can't parse int from '$str'")))

    // Raw domain
    case class RawData(id: String, links: List[RawLink])

    case class RawLink(id: String, mapping: Map[RawLinkKey, RawLinkValue])

    case class RawLinkKey(id: String)

    case class RawLinkValue(value: String)

    // Domain
    case class Data(id: Int, links: List[Link])

    case class Link(id: Int, mapping: Map[LinkKey, LinkValue])

    case class LinkKey(id: Int)

    case class LinkValue(value: Int)

    val rawData = RawData(
      "undefined",
      List(RawLink("null", Map(RawLinkKey("error") -> RawLinkValue("invalid"))))
    )

    // Errors output
    rawData.transformIntoF[V, Data] == Left(
      List(
        TransformationError(
          "Can't parse int from undefined",
          List(Accessor("id"))
        ),
        TransformationError(
          "Can't parse int from null",
          List(Accessor("links"), Index(0), Accessor("id"))
        ),
        TransformationError(
          "Can't parse int from error",
          List(
            Accessor("links"),
            Index(0),
            Accessor("mapping"),
            MapKey(RawLinkKey("error")),
            Accessor("id")
          )
        ),
        TransformationError(
          "Can't parse int from invalid",
          List(
            Accessor("links"),
            Index(0),
            Accessor("mapping"),
            MapValue(RawLinkKey("error")),
            Accessor("value")
          )
        )
      )
    )

    // Using build in showErrorPath
    def printError(err: TransformationError[String]): String =
      s"${err.message} on ${err.showErrorPath}"

    rawData.transformIntoF[V, Data].left.toOption.map(_.map(printError)) ==
      Some(
        List(
          "Can't parse int from undefined on id",
          "Can't parse int from null on links(0).id",
          "Can't parse int from error on links(0).mapping.keys(RawLinkKey(error)).id",
          "Can't parse int from invalid on links(0).mapping(RawLinkKey(error)).value"
        )
      )

Emitted code
------------

Curious how the emitted code for lifted transformers looks like?

Let's first refactor the transformation defined above, which is equivalent to the
previous one, but with few functions extracted out - their implementation is not
really important at this point.

.. code-block:: scala

  def validateEmail(form: RegistrationForm): EitherVecStr[String] = ...
  def computePasswordHash(form: RegistrationForm): String = ...
  def validateAge(form: RegistrationForm): EitherVecStr[Int] = ...

  implicit val transformer: TransformerF[EitherVecStr, RegistrationForm, RegisteredUser] = {
    Transformer.defineF[EitherVecStr, RegistrationForm, RegisteredUser]
      .withFieldComputedF(_.email, validateEmail)
      .withFieldComputed(_.passwordHash, computePasswordHash)
      .withFieldComputedF(_.age, validateAge)
      .buildTransformer
  }

The ``.buildTransformer`` call generates implementation of ``TransformerF``, which is
semantically equivalent to the following, hand-crafted version.

.. code-block:: scala

  implicit val transformer: TransformerF[EitherVecStr, RegistrationForm, RegisteredUser] = {

    val tfs: TransformerFSupport[EitherVecStr] = ... // resolved implicit instance

    new TransformerF[EitherVecStr, RegistrationForm, RegisteredUser] {
      def transform(form: RegistrationForm): EitherVecStr[RegisteredUser] = {
        tfs.map(
          tfs.product(validateEmail(form), validateAge(form)),
          { case (email: String, age: Int) =>
            RegisteredUser(
              email,
              form.username,
              computePasswordHash(form.password),
              age
            )
          }
        )
      }
    }
  }

``tfs.product`` is used to combine results of successful validations into
a tuple type ``(email, age): (String, Int)``. In case that some validations
failed, validation errors are combined together also by ``tfs.product``.

Then, if all validations passed, ``tfs.map`` transforms their results to
a target value of type ``RegisteredUser``. Otherwise, ``tfs.map`` just
passes validation errors as a final result.

.. note::

  - only functions provided by ``withFieldComputedF`` are working with the wrapper
    type ``F``
  - remaining fields transformations (indentity transformer for
    ``username`` and a function provided by ``withFieldComputed`` for ``password``)
    work without any wrapping with ``F``

  This strategy leads to generating particularly efficient code.


.. _deriving-transformerf:

Deriving lifted transformers
----------------------------

When deriving a ``TransformerF[F, From, To]`` instance, where:

- type ``From`` consists of some type ``F1``
- type ``To`` consists of some type ``T1``
- ``F1`` in ``From`` is a counterpart of ``T1`` in ``To``

...we need to have transformation from ``F1`` to ``T1`` in order to be able to
derive requested ``TransformerF``.

The rule is that:

1. we first check for function ``F1 => F[T1]`` passed to lifted DSL
   operations (``withFieldConstF``, ``withFieldComputedF``, etc.)
   or function ``F1 => T1`` passed to total DSL operations
   (``withFieldConst``, ``withFieldComputed``, etc.)

   - whichever was found, it's used in the first place
   - the last one passed in DSL for given field/type wins

2. then we look for implicit instances for ``TransformerF[F, F1, T1]``
   and ``Transformer[F1, T1]``

   - if both of them were found, ambiguity compilation error is reported
   - if only one of them was found, it's used
3. we try to derive lifted ``TransformerF[F, F1, T1]`` using library rules
