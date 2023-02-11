.. _migrating-from-lifted:

Migrating from Lifted Transformers
==================================

Chimney's :ref:`lifted-transformers` were historically first experimental attempt
to express transformations that may potentially fail. Despite their great expressiveness, they were
lacking several basic features and had few design flaws that didn't make them attractive for wider usage.

.. important::

  In case you are using lifted transformers, we strongly recommend migration to :ref:`partial-transformers`.


Design differences
------------------

Let's have a look at type signatures of both lifted and partial transformers.

.. code-block:: scala

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

- Error path support was quite difficult to use in lifted transformers (it required providing another type class
  for your errors collection type), while in partial transformers it is a built-in feature

- In lifted transformers types had a tendency to complicate quickly in a use-site; in partial transformers
  they are conceptually simple, even when using advanced features
