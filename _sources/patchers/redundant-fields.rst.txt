Redundant fields in patchers
============================

When patch case class contains a field that does not exist
in patched object, Chimney will not be able to generate patcher.

.. code-block:: scala

  case class User(id: Int, email: String, phone: Long)
  case class UserUpdateForm(email: String, phone: Long, address: String)

  val user = User(10, "abc@@domain.com", 1234567890L)

  user.patchUsing(UserUpdateForm("xyz@@domain.com", 123123123L, "some address"))
  // Field named 'address' not found in target patching type User

This default behavior is intentional to prevent silent oversight
of typos in patcher field names.

But there is a way to ignore redundant patcher fields explicitly
with ``.ignoreRedundantPatcherFields`` operation.

.. code-block:: scala

  user
    .using(UserUpdateForm2("xyz@@domain.com", 123123123L, "some address"))
    .ignoreRedundantPatcherFields
    .patch
  // User(10, "xyz@@domain.com", 123123123L)

Patching succeeded using only relevant fields that appears in
patched object and ignoring ``address: String`` field from patch.
