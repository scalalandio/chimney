.. _partial-transformers:

Partial transformers
====================

While Chimney transformers represent total functions of type ``From => To``, they don't
really support partial transformations which may either `succeed` or `fail`.

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

We would like to hash the password and parse provided ``age: String`` field into a correct ``Int``
or return an error, when the value is not valid integer. This is not possible using total
``Transformer`` type.

.. code-block:: scala

    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    val okForm = RegistrationForm("john@example.com", "John", "s3cr3t", "40")

    val resultOk = okForm
      .intoPartial[RegisteredUser]
      .withFieldComputed(_.passwordHash, form => hashpw(form.password))
      .withFieldComputedPartial(_.age, form => partial.Result.fromOption(form.age.toIntOption))
      .transform
    // resultOk: partial.Result[RegisteredUser] = Value(
    //   RegisteredUser("john@example.com", "John", "...", 40)
    // )

Partial transformation computes into special result type ``partial.Result``. There are few methods that are
useful to convert such a result to more familiar types.

.. code-block:: scala

    resultOk.asOption // Some(RegisteredUser("john@example.com", "John", "...", 40))
    resultOk.asEither // Right(RegisteredUser("john@example.com", "John", "...", 40))

Capturing errors
----------------

Let's see how partial transformers can handle failure scenarios.

.. code-block:: scala

    val badForm = RegistrationForm("john@example.com", "John", "s3cr3t", "not a number")

    val resultBad = badForm
      .intoPartial[RegisteredUser]
      .withFieldComputed(_.passwordHash, form => hashpw(form.password))
      .withFieldComputedPartial(_.age, form => partial.Result.fromOption(form.age.toIntOption))
      .transform
    // resultBad: partial.Result[RegisteredUser] = Errors(
    //   Iterable(Error(message = EmptyValue, path = ErrorPath(elems = List(Accessor(name = "age")))))
    // )

In this case, we provided illegal string value for field ``age``, which is not parsable to an integer.
The whole transformation now returned error case, providing additional information about the affected fields.

There are few ways how you can access the error information.

.. code-block:: scala

    resultBad.asOption // None
    resultBad.asEither // Left(Errors(Iterable(...)))

    // you can pattern match against the result type
    resultBad match {
      case partial.Result.Value(value) => println(s"transformed to: $value")
      case partial.Result.Errors(errors) => println(s"got ${errors.size} errors")
    }

    // additional convenience methods for quick accessing error information,
    // together with path to the affected field
    resultBad.asErrorPathMessages // List(("age", partial.ErrorMessage.EmptyValue))
    resultBad.asErrorPathMessageStrings // List(("age", "empty value"))

See also :ref:`partial-cats-integration` for other ways of accessing error info.

Custom error messages
---------------------

So far we were receiving `EmptyValue` error, as we just provided ``Option[Int]`` value to
``withFieldComputedPartial``, which can't represent any more detailed error information.

.. code-block:: scala

    import scala.util.{Try, Success, Failure}

    val resultBad2 = badForm
      .intoPartial[RegisteredUser]
      .withFieldComputed(_.passwordHash, form => hashpw(form.password))
      .withFieldComputedPartial(_.age, form => {
        Try(form.age.toInt) match {
          case Success(value) => partial.Result.fromValue(value)
          case Failure(why) => partial.Result.fromErrorString(why.toString)
        }
      })
      .transform
    // resultBad2: partial.Result[RegisteredUser] = Errors(
    //   Iterable(Error(message = StringMessage(""java.lang.NumberFormatException: For input string: \"not a number\"""), path = ErrorPath(elems = List(Accessor(name = "age")))))
    // )

    resultBad2.asErrorPathMessages // List(("age", StringMessage("java.lang.NumberFormatException: For input string: \"not a number\"")))
    resultBad2.asErrorPathMessageStrings // List(("age", "java.lang.NumberFormatException: For input string: \"not a number\""))

Now we wrapped the exception-throwing ``form.age.toInt`` into a ``Try`` and manually propagated detailed error message
to the ``PartialTransformer`` computation. More or less the same result could be achieved using built-in ``Try`` integration.

.. code-block:: scala

      withFieldComputedPartial(_.age, form => partial.Result.fromTry(Try(form.age.toInt)))

      // or catching the exception directly, without Try acting as intermediary
      withFieldComputedPartial(_.age, form => partial.Result.fromCatching(form.age.toInt))

Partial transformers operations
-------------------------------

Similar to ``withFieldConst``, ``withFieldComputed``, ``withCoproductInstance`` operations,
there are partial counterparts available:

- ``withFieldConstPartial``
- ``withFieldComputedPartial``
- ``withCoproductInstancePartial``

Analogously to :ref:`transformer-definition-dsl` for ``Transformer``, we can define above transformation
as implicit ``PartialTransformer[RegistrationForm, RegisteredUser]``. In order to do this,
we use ``PartialTransformer.define`` (or equivalently ``Transformer.definePartial``).

.. code-block:: scala

  implicit val transformer: PartialTransformer[RegistrationForm, RegisteredUser] =
    PartialTransformer.define[RegistrationForm, RegisteredUser]
      .withFieldComputed(_.passwordHash, form => hashpw(form.password))
      .withFieldComputedPartial(_.age, form => partial.Result.fromOption(form.age.toIntOption))
      .buildTransformer

Such an instance may be later picked up and used by other partial transformations.
In the following example it's used for transforming array of registration forms into list of registered users.

.. code-block:: scala

  Array(okForm, badForm).transformIntoPartial[List[RegisteredUser]]
  // ...: partial.Result[List[RegisteredUser]]

Partial transformer result
--------------------------



Short-circuit semantics
-----------------------

Performance notes
-----------------

