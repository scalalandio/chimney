Libraries with smart constructors
=================================

Any type which uses a smart constructor (returning parsed result rather than
throwing an exception) would require partial transformer rather than total
transformer to convert.

If there is no common interface which could be summoned as implicit for
performing smart construction:

.. code-block:: scala

  // assuming Scala 3 or -Xsource:3 for fixed private constructors
  final case class Username private (value: String) extends AnyVal
  object Username {
    def parse(value: String): Either[String, Username] =
      if (value.isEmpty) Left("Username cannot be empty")
      else Right(Username(value))
  }

then partial transformer would have to be created manually:

.. code-block:: scala

   implicit val usernameParse: PartialTransformer[String, Username] =
     PartialTransformer[String, Username] { value =>
       partial.Result.fromEitherString(Username.parse(value))
     }

However, if there was some type class interface, e.g.

.. code-block:: scala

  trait SmartConstructor[From, To] {
    def parse(from: From): Either[String, To]
  }

.. code-block:: scala

  object Username extends SmartConstructor[String, Username] {
    // ...
  }

we could use it to construct ``PartialTransformer`` automatically:

.. code-block:: scala

  implicit def smartConstructedPartial[From, To](
    implicit smartConstructor: SmartConstructor[From, To]
  ): PartialTransformer[From, To] =
     PartialTransformer[From, To] { value =>
       partial.Result.fromEitherString(smartConstructor.parse(value))
     }

The same would be true about extracting values from smart constructed types
(if they are not ``AnyVal``\s, handled by Chimney out of the box).

Let's see how we could implement support for automatic transformations of
types provided in some popular libraries.

Scala NewType
-------------

`NewType <https://github.com/estatico/scala-newtype>`_ is macro-annotation-based
library which attempts to remove runtime overhead from user's types.

.. code-block:: scala

  import io.estatico.newtype.macros.newtype

  @newtype case class Username(value: String)

would be rewritten to become ``String`` in the runtime, while prevent
mixing ``Username`` values with other ``String``\s accidentally.

Newtype ``Coercible`` type class `to allow generic wrapping and unwrapping <https://github.com/estatico/scala-newtype#coercible-instance-trick>`_
of ``@newtype`` values. This type class is not able to validate
casted type, so it safe to use only if NewType is used as a wrapper around
e.g. Refined Type which performs this validation.

.. code-block:: scala

  implicit def newTypeTransformer[From, To](
    implicit coercible: io.estatico.newtype.Coercible[From, To]
  ): Transformer[From, To] = coercible(_)

Monix Newtypes
--------------

`Monix's Newtypes <https://newtypes.monix.io/>`_ is similar to NewType in that
it tries to remove wrapping in runtime. However, it uses different tricks to
achieve it.

.. code-block:: scala

  import monix.newtypes._
  type Username = Username.Type
  object Username extends NewtypeValidated[String] {
    def apply(value: String): Either[BuildFailure[Type], Type] =
      if (value.isEmpty)
        Left(BuildFailure("Username cannot be empty"))
      else
        Right(unsafeCoerce(value))
  }

Additionally it provides 2 type classes: one to extract value
(``HasExtractor``) and one to wrap it (possibly validating, ``HasBuilder``).
We can use them to provide unwrapping ``Transformer`` and wrapping
``PartialTransformer``:

.. code-block:: scala

  implicit def unwrapNewType[Outer, Inner](
    implicit extractor: monix.newtypes.HasExtractor.Aux[Inner, Outer]
  ): Transformer[Outer, Inner] = extractor.extract(_)

  implicit def wrapNewType[Outer, Inner](
    implicit builder: monix.newtype.HasBuilder.Aux[Inner, Outer]
  ): PartialTransformer[Inner, Outer] =  PartialTransformer[Inner, Outer] {
    value =>
      partial.Result.fromEitherString(
        builder.build(value).left.map(_.toReadableString)
      )
  }

Refined Types
-------------

`Refined Types <https://github.com/fthomas/refined>`_ is a library aiming to provide automatic validation of some
popular constraints as long as we express them in value's type.

.. code-block:: scala

  import eu.timepit.refined._
  import eu.timepit.refined.api.Refined
  import eu.timepit.refined.auto._
  import eu.timepit.refined.collections._

  type Username = String Refined NonEmpty

We can validate using dedicated type class (``api.Validate``), while extraction
is a simple accessor:

.. code-block:: scala

  implicit def extractRefined[Type, Refinement]:
      Transformer[Type Refined Refinement, Type] =
    _.value

  implicit def validateRefined[Type, Refinement](
    implicit validate: api.Validate.Plain[Type, Refinement]
  ): PartialTransformer[Type, Type Refined Refinement] =
    PartialTransformer[Type, Type Refined Refinement] {
      value =>
        partial.Result.fromOption(
          validate.validate(value).fold(Some(_), _ => None)
        )
    }
