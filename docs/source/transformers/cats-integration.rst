.. _cats-integration:

Cats integration
================

Chimney provides `Cats <https://typelevel.org/cats>`_ library integration module.
To include it to your SBT project, add the following line to your ``build.sbt``:

.. parsed-literal::

  libraryDependencies += "io.scalaland" %% "chimney-cats" % "|version|"

The module is released for Scala 2.11.x, 2.12.x and 2.13.x and cats 2.0.0.
If you want to use it with Scala.js, you need to replace ``%%`` with ``%%%``.

The module provides package ``io.scalaland.chimney.cats`` with all the goodies
described here.

.. _cats-validated:

``Validated`` support for lifted transformers
---------------------------------------------

Through Chimney cats integration module, you obtain support for
``Validated[EE, +*]``, as the wrapper type for lifted transformers, where:

- ``EE`` - type of an error channel
- ``cats.Semigroup`` implicit instance is available for chosen ``EE`` type

Usual choice for ``EE`` is ``cats.data.NonEmptyChain[String]``.

.. important::

  You need to import ``io.scalaland.chimney.cats._`` in order to use support
  for ``Validated`` type for lifted transformers.


Let's look at the following example.

.. code-block:: scala

  import io.scalaland.chimney.cats._
  import cats.data.{NonEmptyChain, Validated}

  case class RegistrationForm(email: String,
                              username: String,
                              password: String,
                              age: String)

  case class RegisteredUser(email: String,
                            username: String,
                            passwordHash: String,
                            age: Int)

  implicit val transformer: TransformerF[Validated[NonEmptyChain[String], +*], RegistrationForm, RegisteredUser] = {
    Transformer.defineF[Validated[NonEmptyChain[String], +*], RegistrationForm, RegisteredUser]
      .withFieldComputedF(_.email, form => {
        if(form.email.contains('@')) {
          Validated.valid(form.email)
        } else {
          Validated.invalid(NonEmptyChain(s"${form.username}'s email: does not contain '@' character"))
        }
      })
      .withFieldComputed(_.passwordHash, form => hashpw(form.password))
      .withFieldComputedF(_.age, form => form.age.toIntOption match {
        case Some(value) if value >= 18 => Validated.valid(value)
        case Some(value) => Validated.invalid(NonEmptyChain(s"${form.username}'s age: must have at least 18 years"))
        case None => Validated.invalid(NonEmptyChain(s"${form.username}'s age: invalid number"))
      })
      .buildTransformer
  }

Now let's try to use lifted transformers defined above.

.. code-block:: scala

  Array(
    RegistrationForm("john_example.com", "John", "s3cr3t", "10"),
    RegistrationForm("alice@example.com", "Alice", "s3cr3t", "19"),
    RegistrationForm("bob@example.com", "Bob", "s3cr3t", "21.5")
  ).transformIntoF[Validated[NonEmptyChain[String], +*], List[RegisteredUser]]
  // Invalid(
  //   Chain(
  //     "John's email: does not contain '@' character",
  //     "John's age: must have at least 18 years",
  //     "Bob's age: invalid number",
  //   )
  // )

  Array(
    RegistrationForm("john@example.com", "John", "s3cr3t", "40"),
    RegistrationForm("alice@example.com", "Alice", "s3cr3t", "19"),
    RegistrationForm("bob@example.com", "Bob", "s3cr3t", "21")
  ).transformIntoF[Validated[NonEmptyChain[String], +*], List[RegisteredUser]]
  // Valid(
  //   List(
  //     RegisteredUser("john@example.com", "John", "...", 40)
  //     RegisteredUser("alice@example.com", "Alice", "...", 19),
  //     RegisteredUser("bob@example.com", "Bob", "...", 21)
  //   )
  // )

Error path support for cats-based transformers
----------------------------------------------

Chimney provides instance of ``TransformerFErrorPathSupport`` for ``F[_]``
if there is ``ApplicativeError[F, EE[TransformationError[M]]]`` instance and
``Applicative[E]`` instance.

In particular ``ValidatedNec[TransformationError[M], +*]`` and ``ValidatedNel[TransformationError[M], +*]``
satisfy this requirement.

Let's look to example based on ``ValidatedNec[TransformationError[M], +*]``

.. code-block:: scala

  import io.scalaland.chimney.cats._
  import io.scalaland.chimney.dsl._
  import io.scalaland.chimney.{TransformationError, TransformerF}
  import cats.data.{NonEmptyChain, Validated, ValidatedNec}

  import scala.util.Try

  type V[+A] = ValidatedNec[TransformationError[String], A]

  def printError(err: TransformationError[String]): String =
    s"${err.message} on ${err.showErrorPath}"

  implicit val intParse: TransformerF[V, String, Int] =
    str =>
      Validated.fromOption(
        Try(str.toInt).toOption,
        NonEmptyChain.one(TransformationError(s"Can't parse int from $str"))
      )

  // Raw domain
  case class RawClass(id: String, inner: RawInner)

  case class RawInner(id: String, description: String)

  // Domain

  case class Class(id: Int, inner: Inner)

  case class Inner(id: Int, description: String)

  val raw = RawClass("null", RawInner("undefined", "description"))

  raw.transformIntoF[V, Class].leftMap(_.map(printError)) ==
    Validated.Invalid(
      NonEmptyChain(
        "Can't parse int from null on id",
        "Can't parse int from undefined on inner.id"
      )
    )