.. _lifted-cats-integration:

Cats integration ``(deprecated)``
=================================

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

.. _cats-validated:

``Validated`` support for lifted transformers
---------------------------------------------

Through Chimney cats integration module, you obtain support for
``Validated[EE, +*]``, as the wrapper type for lifted transformers, where:

- ``EE`` - type of an error channel
- ``cats.Semigroup`` implicit instance is available for chosen ``EE`` type

Usual choice for ``EE`` is ``cats.data.NonEmptyChain[String]``.


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

.. _cats-ior:

``Ior`` support for lifted transformers
---------------------------------------
Like ``Validated[EE, +*]``, the Chimney cats integration module also supports 
`Ior[EE, +*] <https://typelevel.org/cats/datatypes/ior.html>`_ where:

- ``EE`` - type of an error channel
- ``cats.Semigroup`` implicit instance is available for chosen ``EE`` type

The usual choice for ``EE`` is ``cats.data.NonEmptyChain[String]`` (which has a 
``Semigroup`` typeclass instance).

Let's look at the following example:

.. code-block:: scala

  import io.scalaland.chimney.cats._
  import cats.data.NonEmptyChain

  case class RegistrationForm(email: String,
                              username: String,
                              password: String,
                              age: String)

  case class RegisteredUser(email: String,
                            username: String,
                            passwordHash: String,
                            age: Int)

  implicit val transformer: TransformerF[Ior[NonEmptyChain[String], +*], RegistrationForm, RegisteredUser] =
    Transformer
      .defineF[Ior[NonEmptyChain[String], +*], RegistrationForm, RegisteredUser]
      .withFieldComputedF(
        _.username,
        form =>
          if (form.username.contains(".")) Ior.both(NonEmptyChain(s"${form.username} . is deprecated"), form.username)
          else Ior.right(form.username)
      )
      .withFieldComputedF(
        _.email,
        form => {
          if (form.email.contains('@')) Ior.right(form.email)
          else if (form.username.contains("."))
            Ior.both(NonEmptyChain(s"${form.username} contains . which is deprecated"), form.email)
          else Ior.left(NonEmptyChain(s"${form.username}'s email: does not contain '@' character"))
        }
      )
      .withFieldComputed(_.passwordHash, form => hashpw(form.password))
      .withFieldComputedF(
        _.age,
        form =>
          form.age.toIntOption match {
            case Some(value) if value >= 18 => Ior.right(value)
            case Some(value) if value >= 10 => Ior.both(NonEmptyChain(s"${form.username}: quite young"), value)
            case Some(_)                    => Ior.left(NonEmptyChain(s"${form.username}'s age: must be at least 18 years of age"))
            case None                       => Ior.left(NonEmptyChain(s"${form.username}'s age: invalid number"))
          }
      )
      .buildTransformer

Now let's try to use lifted transformers defined above.

.. code-block:: scala

    Array(
      RegistrationForm("john@example.com", "John.Doe", "s3cr3t", "10"), // Both
      RegistrationForm("alice@example.com", "Alice", "s3cr3t", "19"),   // Right        
      RegistrationForm("bob@example.com", "Bob", "s3cr3t", "21.5")      // Left
    ).transformIntoF[Ior[NonEmptyChain[String], +*], List[RegisteredUser]]
    // Left(
    //  Chain(
    //    "John.Doe . is deprecated",
    //    "John.Doe: quite young",
    //    "Bob's age: invalid number"
    //  )
    // )

As you can see with the example above, we see that ``Ior`` accumulates data on the left side whenever it encounters ``Both`` or ``Right`` 
and will stop accumulating when it encounters a ``Left``.  Let's look at another example:

.. code-block:: scala

    Array(
      RegistrationForm("john@example.com", "John.Doe", "s3cr3t", "40"),
      RegistrationForm("alice@example.com", "Alice", "s3cr3t", "17"),
      RegistrationForm("bob@example.com", "Bob", "s3cr3t", "21")
    ).transformIntoF[Ior[NonEmptyChain[String], +*], List[RegisteredUser]]

    // Both(
    //  Chain(
    //    "John.Doe . is deprecated", 
    //    "Alice: quite young"
    //  ),
    //  List(
    //    RegisteredUser("john@example.com", "John.Doe", "...", 40), 
    //    RegisteredUser("alice@example.com", "Alice", "...", 17), 
    //    RegisteredUser("bob@example.com", "Bob", "...", 21)
    //  )
    // )

In this example, we see that there are no critical errors (i.e. validation's returning only ``Left``) and we see that we end up with a 
result with warnings (``Both``).

Error path support for cats-based transformers
----------------------------------------------

Chimney provides instance of ``TransformerFErrorPathSupport`` for ``F[_]``
if there is ``ApplicativeError[F, EE[TransformationError[M]]]`` instance and
``Applicative[E]`` instance.

In particular ``ValidatedNec[TransformationError[M], +*]``, ``ValidatedNel[TransformationError[M], +*]``,
``IorNec[TransformationError[M], +*]``, ``IorNel[TransformationError[M], +*]``
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
