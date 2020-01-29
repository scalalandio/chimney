Getting started with patchers
=============================

Chimney supports case class patching. It is a bit different type
of transformation when you hold an object of some type, but want
to modify only subset of fields.

Consider following example.

.. code-block:: scala

  case class Email(address: String) extends AnyVal
  case class Phone(number: Long) extends AnyVal

  case class User(id: Int, email: Email, phone: Phone)
  case class UserUpdateForm(email: String, phone: Long)

Let's assume you want to apply update form to existing object
of type ``User``.

.. code-block:: scala

  val user = User(10, Email("abc@@domain.com"), Phone(1234567890L))
  val updateForm = UserUpdateForm("xyz@@domain.com", 123123123L)

  user.patchUsing(updateForm)
  // User(10, Email("xyz@@domain.com"), Phone(123123123L))

Notice that when using patchers, we rely on standard transformers
derivation rules. In this case we used value classes in the
``User`` model, but plain values in update form. Chimney was
able to derive transformers for each patched field, so it was
able to successfully derive a patcher.
