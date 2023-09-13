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

instead of ``io.scalaland.chimney.dsl`` to achieve a similar behavior:

- if you ``import io.scalaland.chimney.syntax.*`` it will expose only extension
  methods working with type classes (``Transformer``, ``PartialTransformer`` and ``Patcher``),
  but with no derivation

- if you ``import io.scalaland.chimney.auto.*`` it will only provide implicit instances
  generated through derivation

- semiautomatic derivation was available for a long time using methods:

  .. code-block:: scala

    // defaults only
    Transformer.derive[From, To]
    PartialTransformer.derive[From, To]
    Patcher.derive[A, Patch]
    // allow customisation
    Transformer.define[From, To].buildTransformer
    PartialTransformer.define[From, To].buildTransformer
    Patcher.define[A, Patch].buildPatcher

- finally, there is ``import io.scalaland.chimney.inlined.*``. It provides extension methods:

  .. code-block:: scala

    from.into[To].transform
    from.intoPartial[To].transform
    from.using[To].patch

  On a first glance, all they do is generate a customized type class before calling it, but
  what actually happens is that it generates an inlined expression, with no type class
  instantiation - if user provided type class for top-level or nested transformation it
  will be used, but wherever Chimney have to generate code ad hoc, it will generate inlined
  code. For that reason this could be considered a third mode, one where generated code
  is non-reusable, but optimized to avoid any type class allocation and deferring
  ``partial.Result`` wrapping (in case of ``PartialTransformer`` s) as long as possible.

Performance concerns
--------------------

When Chimney derives an expression, whether that is an expression directly inlined at call site
or as body of the ``transform``/``patch`` method inside a type class instance, it attempts
to generate a fast code.

It contains a special cases for ``Option`` s, ``Either`` s, it attempt to avoid boxing with
``partial.Result`` and creating type classes if it can help it.

You can use ``.enableMacrosLogging`` to see the code generated by

.. code-block:: scala

  case class Foo(baz: Foo.Baz)
  object Foo {
    case class Baz(a: String)
  }
  case class Bar(baz: Bar.Baz)
  object Bar {
    case class Baz(a: String)
  }

  Foo(Foo.Baz("string")).into[Bar].enableMacrosLogging.transform

The generated code (in the absence of implicits) should be

.. code-block:: scala

  val foo = Foo(Foo.Baz("string"))
  new Bar(new Bar.Baz(foo.baz.a))

Similarly, when deriving a type class it would be

.. code-block:: scala

  new Transformer[Foo, Bar] {
    def transform(foo: Foo): Bar =
      new Bar(new Bar.Baz(foo.baz.a))
  }

However, Chimney is only able to do it when given a free reign. It checks
if user provided an implicit, and if they did, it should be used instead.

In case of the automatic derivation, it means that every single branching
in the code - derivation for a field of a case class, or a subtype of a
sealed hierarchy - will trigger a macro, which may or mey not succeed
and it it will succeed it will introduce an allocation.

When using ``import io.scalaland.chimney.dsl.*`` this is countered by the usage of
a ``Transformer.AutoDerived`` as a supertype of ``Transformer`` - automatic
derivation upcast ``Transformer`` and recursive construction of an expression requires
a normal ``Transformer`` so automatic derivation is NOT triggered. Either the user provided
an implicit or there is none.

However, with ``import io.scalaland.chimney.auto.*`` the same semantics as in other
libraries is used: implicit def returns ``Transformer``, so if derivation with defaults
is possible it will be always triggered.

The matter is even more complex with ``PartialTransformer`` s - they look for both implicit
``Transformer`` s as well as implicit ``PartialTransformer`` s (users can provide either or both).
With the automatic derivation both versions could always be available, so users need to always
provide ``implicitConflictResolution`` flag.

For the reasons above the recommendations are as follows:

- if you care about performance use either inline derivation (for a one-time-usage) or
  semiautomatic derivation (``.derive``/``.define.build*`` + ``syntax.*``)
- only use ``import auto.*`` when you want predictable behavior similar to other libraries
  (predictably bad)
- if you use unit test to ensure that you code does what it should and benchmarks to
  ensure it is reasonably fast keep on using ``import dsl.*``
