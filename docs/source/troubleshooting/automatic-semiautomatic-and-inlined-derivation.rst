Automatic, semiautomatic and inlined derivation
===============================================

When you use the standard way of working with Chimney, but ``import io.scalaland.chimney.dsl.*``
you might notice that it is very convenient approach, making a lot of things easy:

- when you want to trivially convert ``val from: From`` into ``To`` you can do
  it with ``from.transformInto[To]``
- the code above would be able to map case classes recursively
- if you wanted to provide some transformation to use either directly in this
  ``.transformInto`` or in some nesting, you can do it just by using implicits
- if you wanted to generate this implicit you could use ``Transformer.derive``
- if you needed to customize the derivation you could us
  ``Transformer.define.customisationMethod.buildTransformer`` or
  ``from.into[To].customisationMethod.transform``

However, sometime you may want to restrict this behavior. It might be too easy to:

- derive the same transformation again and again
- define some customized ``Transformer``, not import it by accident and still
  end up with compiling code since Chimney could derive a new one on the spot

Automatic vs semiautomatic
--------------------------

In other libraries this issue is addressed by providing 2 flavours of derivation:

- automatic derivation: usually requires some ``import library.auto.*``, allows you
  to get a derived instance just by summoning it e.g. with ``implicitly[TypeClass[A]]``
  or calling any other method which would take it as ``implicit`` parameter.

  Usually, it is convenient to use, but has a downside of re-deriving the same instance
  each time you need it. Additionally, you cannot write

  .. code-block:: scala

    implicit val typeclass: TypeClass[A] = implicitly[TypeClass[A]]

  since that generates circular dependency on a value initialisation. This makes it hard
  to cache this instance in e.g. companion object. In some libraries it also makes it hard
  to use automatic derivation to work with recursive data structures.

- semiautomatic derivation: require you to explicitly call some method which will provide
  a derived instance. It has the downside that each instance that you would like to summon
  you need to manually derive and assign to an ``implicit val`` or ``def``

  .. code-block:: scala

  implicit val typeclass: TypeClass[A] = deriveTypeClass[A]

  However, it gives you certainty that each time you need an instance of a type class
  it will be the one you manually created. It reduces compile time, and make it easier
  to limit the places where error can happen (if you reuse the same instance everywhere
  and there is a bug in an instance, there is only one place to look for it).

The last property is a reason many projects encourage usage of semiautomatic derivation
and many libraries provide automatic derivation as quick and dirty way of doing things
requiring an opt-in.

Chimney's defaults for (good) historical reasons mix these 2 modes (and one more, which
will describe in a moment), but it also allows you to selectively use these imports

.. code-block:: scala

  import io.scalaland.chimney.auto.*
  import io.scalaland.chimney.inlined.*
  import io.scalaland.chimney.syntax.*

instead of ``io.scalaland.chimney.dsl`` to achieve a similar behavior.

TODO: auto.* and syntax.*
