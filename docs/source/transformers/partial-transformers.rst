.. _partial-transformers:

Partial transformers
====================

While Chimney transformers wrap total functions of type ``From => To``, they don't
really support partial transformations, where depending on the input value, transformation
may `succeed` or `fail`.


Motivating example
------------------

Let's take a look at the following example.

.. code-block:: scala

  case class RegistrationForm(email: String,
                              username: String,
                              password: String,
                              age: String)

  case class RegisteredUser(email: String,
                            username: String,
                            passwordHash: String,


Partial transformation operations
---------------------------------


Partial transformer result
--------------------------


Built-in error path support
---------------------------


.. warning::

    Support for enhanced error paths is currently an experimental feature and we don't
    guarantee it will be included in the next library versions in the same shape.


Short-circuit semantics
-----------------------

Migrating from lifted transformers
----------------------------------

Note on performance of generated code
-------------------------------------


Integration with Cats library
-------------------------------------






