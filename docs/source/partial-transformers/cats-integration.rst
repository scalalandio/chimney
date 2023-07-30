.. _partial-cats-integration:

Cats integration
================

Chimney provides `Cats <https://typelevel.org/cats>`_ library integration module
for partial transformers.
To include it to your sbt project, add the following line to your ``build.sbt``:

.. parsed-literal::

  libraryDependencies += "io.scalaland" %% "chimney-cats" % "|version|"

The module is released for Scala 2.12.x, 2.13.x, 3.3.x and cats 2.9.x.
If you want to use it with Scala.js, you need to replace ``%%`` with ``%%%``.

The module provides package ``io.scalaland.chimney.cats`` with all the goodies
described here.

Contents
--------

Cats integration module contains the following stuff:

* type classes instances for partial transformers data structures

    * ``Applicative`` instance for ``partial.Result``
    * ``Semigroup`` instance for ``partial.Result.Errors``

* integration with ``Validated`` (and ``ValidatedNel``, ``ValidatedNec``) data type for partial transformers

.. important::

  You need to import ``io.scalaland.chimney.cats._`` in order to have all the above in scope.

Example
-------

Let's have a look at how to integrate :ref:`partial-transformers` with Cats' ``Validated``.

.. code-block:: scala

  case class RegistrationForm(email: String,
                              username: String,
                              password: String,
                              age: String)

  case class RegisteredUser(email: String,
                            username: String,
                            passwordHash: String,
                            age: Int)

  import io.scalaland.chimney._
  import io.scalaland.chimney.dsl._
  import io.scalaland.chimney.partial
  import io.scalaland.chimney.cats._
  import cats.data._

  def validateEmail(form: RegistrationForm): ValidatedNec[String, String] = {
    if(form.email.contains('@')) {
      Validated.valid(form.email)
    } else {
      Validated.invalid(NonEmptyChain(s"${form.username}'s email: does not contain '@' character"))
    }
  }

  def validateAge(form: RegistrationForm): ValidatedNec[String, Int] = form.age.toIntOption match {
    case Some(value) if value >= 18 => Validated.valid(value)
    case Some(value) => Validated.invalid(NonEmptyChain(s"${form.username}'s age: must have at least 18 years"))
    case None => Validated.invalid(NonEmptyChain(s"${form.username}'s age: invalid number"))
  }

  implicit val partialTransformer: PartialTransformer[RegistrationForm, RegisteredUser] =
    PartialTransformer
      .define[RegistrationForm, RegisteredUser]
      .withFieldComputedPartial(_.email, form => validateEmail(form).toPartialResult)
      .withFieldComputed(_.passwordHash, form => hashpw(form.password))
      .withFieldComputedPartial(_.age, form => validateAge(form).toPartialResult)
      .buildTransformer

  val okForm = RegistrationForm("john@example.com", "John", "s3cr3t", "40")
  okForm.transformIntoPartial[RegisteredUser].asValidatedNec
  // Valid(RegisteredUser(email = "john@example.com", username = "John", passwordHash = "...", age = 40))

  Array(
    RegistrationForm("john_example.com", "John", "s3cr3t", "10"),
    RegistrationForm("alice@example.com", "Alice", "s3cr3t", "19"),
    RegistrationForm("bob@example.com", "Bob", "s3cr3t", "21.5")
  ).transformIntoPartial[Array[RegisteredUser]].asValidatedNel
  // Invalid(NonEmptyList(
  //   Error(StringMessage("John's email: does not contain '@' character"), ErrorPath(List(Index(0), Accessor("email")))),
  //   Error(StringMessage("John's age: must have at least 18 years"), ErrorPath(List(Index(0), Accessor("age")))),
  //   Error(StringMessage("Bob's age: invalid number"), ErrorPath(List(Index(2), Accessor("age"))))
  // ))


Form validation logic is implemented in terms of ``Validated`` data type. You can easily convert
it to a ``partial.Result`` required by ``withFieldComputedPartial`` by just using ``.toPartialResult``
which is available after importing the cats integration utilities (``import io.scalaland.chimney.cats._``).

Result of the partial transformation is then converted to ``ValidatedNel`` or ``ValidatedNec`` using either
``.asValidatedNel`` or ``.asValidatedNec`` extension method call.
