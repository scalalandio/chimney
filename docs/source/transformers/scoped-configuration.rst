.. _scoped-configuration:

Providing scoped configuration
==============================

Chimney is highly configurable and has some transformer derivation rules that
can be switched on/off on demand, for each specific transformation. However sometimes
you may find yourself enabling (or disabling) the same flags for majority of
transformations in your project. This is when you may consider using providing
scoped transformer configurations.

Motivating example
------------------

Let's consider a following example.

.. code-block:: scala

    class Source { def field1: Int = 100 }
    case class Target(field1: Int, field2: Option[String])

    (new Source)
        .into[Target]
        .enableMethodAccessors
        .enableOptionDefaultsToNone
        .transform
    // Target(100, None)

In order to make it working without providing any own specific values, we must
enable :ref:`method accessors<Using method accessors>` and
:ref:`None as Option default value<Default values for \`\`Option\`\` fields>`.

Instead of enabling them per use-site, we can define default transformer configuration
in implicit scope.

.. code-block:: scala

    implicit val myTransformerConfig =
        TransformerConfiguration.default
            .enableMethodAccessors
            .enableOptionDefaultsToNone

    (new Source)
        .into[Target]
        .transform
    // Target(100, None)

    (new Source).transformInto[Target]
    // Target(100, None)

Then, in scope where ``TransformerConfiguration`` is available, Chimney is able to
pick it up as a base configuration for transformer derivation. This way you can define
your default transformers ruleset for your module or project.

Overriding scoped configuration locally
---------------------------------------

Use-site override of scoped configuration is still possible, as in the following examples.

.. code-block:: scala

    implicit val myTransformerConfig =
        TransformerConfiguration.default
            .enableMethodAccessors
            .enableOptionDefaultsToNone

    (new Source)
        .into[Target]
        .disableMethodAccessors
        .transform
    // error: Chimney can't derive transformation from Source to Target
    // Target
    //   field1: scala.Int - no accessor named field1 in source type Source

We locally disabled previously scope-enabled method accessors, which results in
compilation error. It's expected as the local override has priority over default,
scoped configuration.

.. code-block:: scala

    implicit val myTransformerConfig =
        TransformerConfiguration.default
            .enableMethodAccessors

    (new Source)
        .into[Target]
        .enableOptionDefaultsToNone
        .transform
    // Target(100, None)

This way we can also enable flags partially. The final configuration is merged from
scoped one and any (eventual) local one.
