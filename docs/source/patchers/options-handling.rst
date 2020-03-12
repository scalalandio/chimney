Handling optional fields
========================

It is possible to patch using optional values of type ``Option[T]``
as long as the transformer is available for ``T``. If the value
is present (``Some``), it's used for patching a field in the target object;
otherwise (``None``) it's ignored and the field value is copied from
the original object.

Let's consider the following patch.

.. code-block:: scala

  case class User(id: Int, email: String, phone: Long)
  case class UserPatch(email: Option[String], phone: Option[Long])

Then it is possible to patch as follows:

.. code-block:: scala

  val user = User(10, "abc@@domain.com", 1234567890L)
  val update = UserPatch(email = Some("updated@@example.com"), phone = None)

  user.patchUsing(update)
  //  User(10, "updated@@example.com", 1234567890L)

Field ``phone`` remained the same as in the original ``user``, while
the optional e-mail string got updated from a patch object.

``Option[T]`` on both sides
---------------------------

An interesting case appears when both patch case class and patched
object define fields ``f: Option[T]``. Depending on values
of ``f`` in patched object and patch, we would like to
apply following semantic table.

+---------------------+------------------+------------------+
| ``patchedObject.f`` | ``patch.f``      | patching result  |
+=====================+==================+==================+
| ``None``            | ``Some(value)``  | ``Some(value)``  |
+---------------------+------------------+------------------+
| ``Some(value1)``    | ``Some(value2)`` | ``Some(value2)`` |
+---------------------+------------------+------------------+
| ``None``            | ``None``         | ``None``         |
+---------------------+------------------+------------------+
| ``Some(value)``     | ``None``         | **???**          |
+---------------------+------------------+------------------+

When a ``patch.f`` contains some value, it's immediately used for
replacing field in target object (rows 1 and 2), regardless of original
object field value. When both field are ``None``, patching result
is also ``None`` (row 3).

But if original object contains a some value, but patch comes
with a ``None``, we can do two things:

- clear value in target object (replace it with ``None``)
- or ignore updating this particular field (as in previous section)

Both choices may have perfect sense, depending on the context.
By default, Chimney does the former (clears the value), but it
also gives a simple way to always ignore ``None`` from patch
with ``.ignoreNoneInPatch`` operation.

.. code-block:: scala

  case class User(name: Option[String], age: Option[Int])
  case class UserPatch(name: Option[String], age: Option[Int])

  val user = User(Some("John"), Some(30))
  val userPatch = UserPatch(None, None)

  user.patchUsing(userPatch)
  // clears both fields: User(None, None)

  user
    .using(userPatch)
    .ignoreNoneInPatch
    .patch
  // ignores updating both fields: User(Some("John"), Some(30))
