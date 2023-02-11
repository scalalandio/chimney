.. _partial-transformers:

Partial transformers
====================

While Chimney transformers wrap total functions of type ``From => To``, they don't
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

    okForm
      .intoPartial[RegisteredUser]
      .withFieldComputed(_.passwordHash, form => hashpw(form.password))
      .withFieldComputedPartial(_.age, form => partial.Result.fromOption(form.age.toIntOption))
      .transform
    // partial.Result.Value(
    //   RegisteredUser("john@example.com", "John", "...", 40)
    // ): Option[RegisteredUser]


Partial transformer result
--------------------------


Errors accumulation
-------------------


Short-circuit semantics
-----------------------

Performance notes
-----------------

