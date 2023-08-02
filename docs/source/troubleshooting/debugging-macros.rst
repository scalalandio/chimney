Debugging macros
================

In some cases it could be helpful to preview what is the expression generated
by macros, which implicits were used in macro (or not) and what was the exact
logic that lead to the final expression or compilation errors.

In such cases, we can use a dedicated flag, ``.enableMacrosLogging``:

.. code-block:: scala

  import io.scalaland.chimney.dsl._

  case class Foo(x: String, y: Int, z: Boolean = true)
  case class Bar(x: String, y: Int)

  Bar("abc", 10).into[Foo].enableDefaultValues.enableMacrosLogging.transform

For the snippet above, the macro could print this structured log:

.. code-block::

  + Start derivation with context: ForTotal[From = Bar, To = Foo](src = bar)(TransformerConfig(
  |   flags = Flags(processDefaultValues, displayMacrosLogging),
  |   fieldOverrides = Map(),
  |   coproductOverrides = Map(),
  |   preventResolutionForTypes = None
  | ))
  + Deriving Total Transformer expression from Bar to Foo
    + Attempting expansion of rule Implicit
    + Rule Implicit decided to pass on to the next rule
    + Attempting expansion of rule Subtypes
    + Rule Subtypes decided to pass on to the next rule
    + Attempting expansion of rule OptionToOption
    + Rule OptionToOption decided to pass on to the next rule
    + Attempting expansion of rule PartialOptionToNonOption
    + Rule PartialOptionToNonOption decided to pass on to the next rule
    + Attempting expansion of rule ToOption
    + Rule ToOption decided to pass on to the next rule
    + Attempting expansion of rule ValueClassToValueClass
    + Rule ValueClassToValueClass decided to pass on to the next rule
    + Attempting expansion of rule ValueClassToType
    + Rule ValueClassToType decided to pass on to the next rule
    + Attempting expansion of rule TypeToValueClass
    + Rule TypeToValueClass decided to pass on to the next rule
    + Attempting expansion of rule EitherToEither
    + Rule EitherToEither decided to pass on to the next rule
    + Attempting expansion of rule MapToMap
    + Rule MapToMap decided to pass on to the next rule
    + Attempting expansion of rule IterableToIterable
    + Rule IterableToIterable decided to pass on to the next rule
    + Attempting expansion of rule ProductToProduct
      + Resolved Bar getters: (`x`: java.lang.String (ConstructorVal), `y`: scala.Int (ConstructorVal)) and Foo constructor (`x`: java.lang.String (ConstructorParameter, default = None), `y`: scala.Int (ConstructorParameter, default = None), `z`: scala.Boolean (ConstructorParameter, default = Some(Foo.apply$default)))
      + Recursive derivation for field `x`: java.lang.String into matched `x`: java.lang.String
        + Deriving Total Transformer expression from java.lang.String to java.lang.String
          + Attempting expansion of rule Implicit
          + Rule Implicit decided to pass on to the next rule
          + Attempting expansion of rule Subtypes
          + Rule Subtypes expanded successfully: bar.x
        + Derived recursively total expression bar.x
      + Resolved `x` field value to bar.x
       + Recursive derivation for field `y`: scala.Int into matched `y`: scala.Int
        + Deriving Total Transformer expression from scala.Int to scala.Int
          + Attempting expansion of rule Implicit
          + Rule Implicit decided to pass on to the next rule
          + Attempting expansion of rule Subtypes
          + Rule Subtypes expanded successfully: bar.y
        + Derived recursively total expression bar.y
      + Resolved `y` field value to bar.y
      + Resolved `z` field value to Foo.apply$default
      + Resolved 3 arguments, 3 as total and 0 as partial Expr
    + Rule ProductToProduct expanded successfully:
    | new Foo(bar.x, bar.y, Foo.apply$default)
  + Derived final expression is:
  | new Foo(bar.x, bar.y, Foo.apply$default)
  + Derivation took 0.109828000 s

With the structured log user could see e.g.:

- that no implicit was summoned during the expansion
- how ``Foo`` constructor was called
- that default values was used and how it was obtained
- what is the final expression and how long it took to compute it

.. warning::

  Structured logs from macros are still logs - their role is to help with
  debugging, but their format evolves over time and log for one macro could
  look completely different to log from another macro. Examples from this
  page should not be treated as any point of reference.

Enabling logs can be done both on an individual transformation level, like
above, or with a shared implicit config:

.. code-block:: scala

  implicit val cfg = TransformerConfiguration.default.enableMacrosLogging

The flag is also available to ``Patcher``\s, this code:

.. code-block:: scala

  case class Email(address: String) extends AnyVal
  case class Phone(number: Long) extends AnyVal

  case class User(id: Int, email: Email, phone: Phone)
  case class UserUpdateForm(email: String, phone: Long)

  val user = User(10, Email("abc@@domain.com"), Phone(1234567890L))
  val updateForm = UserUpdateForm("xyz@@domain.com", 123123123L)

  user.using(updateForm).enableMacrosLogging.patch

would generate:

.. code-block::

  + Deriving Patcher expression for User with patch UserUpdateForm
    + Deriving Total Transformer expression from java.lang.String to Email
      + Attempting expansion of rule Implicit
      + Rule Implicit decided to pass on to the next rule
      + Attempting expansion of rule Subtypes
      + Rule Subtypes decided to pass on to the next rule
      + Attempting expansion of rule OptionToOption
      + Rule OptionToOption decided to pass on to the next rule
      + Attempting expansion of rule PartialOptionToNonOption
      + Rule PartialOptionToNonOption decided to pass on to the next rule
      + Attempting expansion of rule ToOption
      + Rule ToOption decided to pass on to the next rule
      + Attempting expansion of rule ValueClassToValueClass
      + Rule ValueClassToValueClass decided to pass on to the next rule
      + Attempting expansion of rule ValueClassToType
      + Rule ValueClassToType decided to pass on to the next rule
      + Attempting expansion of rule TypeToValueClass
        + Deriving Total Transformer expression from java.lang.String to java.lang.String
          + Attempting expansion of rule Implicit
          + Rule Implicit decided to pass on to the next rule
          + Attempting expansion of rule Subtypes
          + Rule Subtypes expanded successfully: userupdateform.email
        + Derived recursively total expression userupdateform.email
      + Rule TypeToValueClass expanded successfully: new Email(userupdateform.email)
    + Deriving Total Transformer expression from scala.Long to Phone
      + Attempting expansion of rule Implicit
      + Rule Implicit decided to pass on to the next rule
      + Attempting expansion of rule Subtypes
      + Rule Subtypes decided to pass on to the next rule
      + Attempting expansion of rule OptionToOption
      + Rule OptionToOption decided to pass on to the next rule
      + Attempting expansion of rule PartialOptionToNonOption
      + Rule PartialOptionToNonOption decided to pass on to the next rule
      + Attempting expansion of rule ToOption
      + Rule ToOption decided to pass on to the next rule
      + Attempting expansion of rule ValueClassToValueClass
      + Rule ValueClassToValueClass decided to pass on to the next rule
      + Attempting expansion of rule ValueClassToType
      + Rule ValueClassToType decided to pass on to the next rule
      + Attempting expansion of rule TypeToValueClass
        + Deriving Total Transformer expression from scala.Long to scala.Long
          + Attempting expansion of rule Implicit
          + Rule Implicit decided to pass on to the next rule
          + Attempting expansion of rule Subtypes
          + Rule Subtypes expanded successfully: userupdateform.phone
        + Derived recursively total expression userupdateform.phone
      + Rule TypeToValueClass expanded successfully: new Phone(userupdateform.phone)
  + Derived final expression is:
  | {
  |   val user: User = new User(user.id, new Email(userupdateform.email), new Phone(userupdateform.phone));
  |   user
  | }
  + Derivation took 0.064756000 s
