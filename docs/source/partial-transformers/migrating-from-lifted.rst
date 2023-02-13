.. _migrating-from-lifted:

Migrating from Lifted Transformers
==================================

Chimney's :ref:`lifted-transformers` were historically first experimental attempt
to express transformations that may potentially fail. Despite their great expressiveness, they were
lacking several basic features and had a few design flaws that make them unattractive/difficult
for wider adoption.

.. important::

  In case you are using lifted transformers, we strongly recommend migration to :ref:`partial-transformers`.


Design differences
------------------

Let's have a look at type signatures of both lifted and partial transformers.

.. code-block:: scala

    package io.scalaland.chimney

    // lifted transformer
    trait TransformerF[F[+_], From, To] {
      def transform(src: From): F[To]
    }

    // partial transformer
    import io.scalaland.chimney.partial
    trait PartialTransformer[From, To] {
       def transform(src: From, failFast: Boolean): partial.Result[To]
    }

- Lifted transformers provided abstraction over the target transformation type container (``F[+_]``), while
  partial transformers fix resulting type to built-in ``partial.Result[_]``

  - as a consequence of this abstraction, lifted transformers required a type class instance
    (``TransformerFSupport``) in scope for every specific ``F[_+]`` used

  - partial transformers rely on built-in behavior and provide convenience methods to convert between more familiar
    data types (``Option``, ``Either``, etc.)

  - abstraction over the resulting container type in lifted transformer allowed for having custom error types;
    this is not easily possible with partial transformer, which focuses on few most common error types

- Partial transformer has built-in support for fail fast (short circuiting) semantics by passing ``failFast``
  boolean parameter, while in lifted transformers it was barely possible (only by providing supporting type class
  that had such a fixed behavior)

- Error path support in lifted transformers required providing another type class instance
  (``TransformerFErrorPathSupport``) for your errors collection type,
  while in partial transformers it is a built-in feature

Migrating your code
-------------------

In order to migrate your code from lifted transformers to partial transformers, you may take the following steps.

#. Replace all the occurrences of ``TransformerF`` type with ``PartialTransformer`` and remove the first type argument
   (``F[_]``) which is not used for partial transformers.

#. For your transformations find corresponding DSL methods. Their name usually differ on suffix, for example:

    * replace ``withFieldConstF`` with ``withFieldConstPartial``

    * replace ``withFieldComputedF`` with ``withFieldComputedPartial``

    * etc.

#. Adjust the types passed to the customization methods. In lifted transformers they were expecting values of
   your custom type ``F[T]``, while in partial transformers they work with ``partial.Result[T]``. See the
   ``partial.Result`` companion object for ways of constructing `success` and `failure` instances, for example:

    * ``Result.fromValue``

    * ``Result.fromOption``

    * ``Result.fromEither``

    * ``Result.fromTry``

    * and so on...

#. Resulting type of a call to ``.transform`` is also a ``partial.Result[T]``. If you don't want to work with
   ``partial.Result`` directly, figure out ways how to convert it to other, more familiar data structures.
   Some of the ways may include:

    * ``result.asOption``

    * ``result.asEither``

    * ``result.asErrorPathMessages``

    * other data structures using :ref:`partial-cats-integration`

.. TODO: provide github links to the paragraph below

See also our `test suites <https://github.com/scalalandio/chimney/tree/master/chimney/src/test/scala/io/scalaland/chimney>`_
for lifted transformers and partial transformers to get an idea how similar behavior might be implemented
using both features.
