Welcome to Chimney's documentation!
===================================

**Chimney** is a Scala library for boilerplate-free data transformations.

Imagine you have strict domain definition of a ``User`` and much less strict
API definition of a ``UserAPI``:

.. code-block:: scala

  case class User(name: User.Name, surname: User.Surname)
  object User {
    case class Name(value: String) extends AnyVal
    case class Surname(value: String) extends AnyVal
  }

  case class UserAPI(name: Option[String], surname: Option[String])

Converting the strict representation to less strict is obvious and boring:

.. code-block:: scala

  val user = User(User.Name("John"), User.Surname("Smith"))

  // encoding domain to API by hand
  val userApi = UserAPI(Some(user.name.value), Some(user.surname.value))

Converting the less strict representation to strict is also obvious and boring,
and additionally long:

.. code-block:: scala

  val userApi = UserAPI(Some(user.name.value), Some(user.surname.value))

  // decoding API to domain by hand
  val userOpt = for {
    name <- user.name.map(User.Name)
    surname <- user.surname.map(User.Surname)
  } yield User(name, surname)

The good news is that this obvious and boring code could be generated for you:

.. code-block:: scala

  import io.scalaland.chimney.dsl._

  user.transformInto[UserAPI]
  // UserAPI("John", "Smith")

  userApi.transformIntoPartial[User].asOption
  // Some(User(Name(John, Surname(Smith))))

Simple and easy! And no need to worry that you forgot to change something
as you copy-pasted pieces of the transformation ad nauseam!

It can also be generated for you when you works with sealed hierarchies
(including Scala 3's enum):

.. code-block:: scala

  sealed trait UserStatusAPI
  object UserStatusAPI {
    case object Active extends UserStatusAPI
    case class Inactive(cause: String) extends UserStatusAPI
  }

  enum UserStatus:
    case Active
    case Inactive(cause: String)

  val userStatusAPI: UserStatusAPI = UserStatusAPI.Active
  val userStatus: UserStatus = UserStatus.Inactive("banned")

.. code-block:: scala

  import io.scalaland.chimney.dsl._

  userStatusApi.transformInto[UserStatus]
  // UserStatus.Active

  userStatus.transformInto[UserStatusAPI]
  // UserStatusAPI.Inactive(banned)

and Java Beans:

.. code-block:: scala

  class UserBean {
    private var name: String = _
    private var surname: String = _

    def getName: String = name
    def setName(name: String): Unit = this.name = name

    def getSurname: String = surname
    def setSurname(surname: String): Unit = this.surname = surname

    override def toString(): String = s"UserBean($name, $surname)"
  }

  val userBean = new UserBean()
  userBean.setName("John")
  userBean.setSurname("Smith")

.. code-block:: scala

  import io.scalaland.chimney.dsl._

  user.into[UserBean].enableBeanSetters.transform
  // UserBean(John, Smith)

  userBean.into[User].enableBeanGetters.transform
  // User(John, Smith)

With Chimney, this and much more can be safely generated for you by the compiler -
the more repetitive transformation you have the more boilerplate you will shrug off!

Transformers
------------

In the daily life of a strongly-typed language's programmer sometimes it
happens we need to transform an object of one type to another object which
contains a number of the same or similar fields in their definitions.

.. code-block:: scala

  case class MakeCoffee(id: Int, kind: String, addict: String)
  case class CoffeeMade(id: Int, kind: String, forAddict: String, at: ZonedDateTime)

Usual approach is to just rewrite fields one by one

.. code-block:: scala

  val command = MakeCoffee(id = Random.nextInt,
                           kind = "Espresso",
                           addict = "Piotr")
  // MakeCoffee(24, "Espresso", "Piotr")

  val event = CoffeeMade(id = command.id,
                         kind = command.kind,
                         forAddict = command.addict,
                         at = ZonedDateTime.now)
  // CoffeeMade(24, "Espresso", "Piotr", "2020-02-03T20:26:59.659647+07:00[Asia/Bangkok]")


While the example stays short, in real-life code we usually end up with tons
of such boilerplate, especially when:

- we keep separate models of different layers in the system
- we apply practices like DDD (Domain-Driven-Design) where suggested
  approach is to separate model schemas of different bounded contexts
- we use code-generation tools like Protocol Buffers that generate primitive
  types like ``Int`` or ``String``, while you'd prefer to use value objects
  in your domain-level code to improve type-safety and readability
- we maintain typed, versioned schemas and want to migrate between multiple schema versions

Chimney provides a compact DSL with which you can define transformation
rules and transform your objects with as little boilerplate as possible.

.. code-block:: scala

  import io.scalaland.chimney.dsl._

  val event = command.into[CoffeeMade]
    .withFieldComputed(_.at, _ => ZonedDateTime.now)
    .withFieldRenamed(_.addict, _.forAddict)
    .transform
  // CoffeeMade(24, "Espresso", "Piotr", "2020-02-03T20:26:59.659647+07:00[Asia/Bangkok]")


Read :ref:`transformers/getting-started:Getting started with transformers` to learn more about
Chimney's transformers.

Partial transformers
--------------------

For computations that may potentially fail, Chimney provides partial transformers.

.. code-block:: scala

  import io.scalaland.chimney.dsl._
  import io.scalaland.chimney.partial._

  case class UserForm(name: String, ageInput: String, email: Option[String])
  case class User(name: String, age: Int, email: String)

  UserForm("John", "21", Some("john@example.com"))
    .intoPartial[User]
    .withFieldComputedPartial(_.age, form => Result.fromCatching(form.ageInput.toInt))
    .transform
    .asOption  // Some(User("name", 21, "john@example.com"))

  val result = UserForm("Ted", "eighteen", None)
    .intoPartial[User]
    .withFieldComputedPartial(_.age, form => Result.fromCatching(form.ageInput.toInt))
    .transform

  result.asOption // None
  result.asErrorMessageStrings
  // Iterable("age" -> "For input string: \"eighteen\"", "email" -> "empty value")

Read :ref:`partial-transformers/partial-transformers:Partial transformers` to learn more about
Chimney's partial transformers.

Patching
--------

Beside transformers, Chimney supports case class patching as well.
It's a bit different type of transformation which happens when you
hold an object of some type and want to modify only subset of its
fields with values taken from other (*patch*) object.

.. code-block:: scala

  case class User(id: Int, email: String, address: String, phone: Long)
  case class UserUpdateForm(email: String, phone: Long)

  val user = User(10, "abc@example.com", "Broadway", 123456789L)
  val updateForm = UserUpdateForm("xyz@example.com", 123123123L)

  user.patchUsing(updateForm)
  // User(10, "xyz@example.com", "Broadway", 123123123L)

Read :ref:`patchers/getting-started:Getting started with patchers` to learn more about
Chimney's patchers.

Chimney's features
------------------

Chimney uses Scala macros to give you:

- type-safety at compile-time
- fast generated code, almost equivalent to hand-written version
- excellent error messages
- minimal overhead on compilation time
- a way to express custom transformation logic

Contents:
=========

.. toctree::
   :maxdepth: 2
   :caption: Getting started

   getting-started/quickstart
   getting-started/community

.. toctree::
   :maxdepth: 2
   :caption: Transformers

   transformers/getting-started
   transformers/customizing-transformers
   transformers/standard-transformers
   transformers/default-values
   transformers/own-transformations
   transformers/scoped-configuration
   transformers/java-beans

.. toctree::
   :maxdepth: 2
   :caption: Partial Transformers

   partial-transformers/partial-transformers
   partial-transformers/migrating-from-lifted
   partial-transformers/total-vs-partial-conflicts
   partial-transformers/cats-integration

.. toctree::
   :maxdepth: 2
   :caption: Patchers

   patchers/getting-started
   patchers/redundant-fields
   patchers/options-handling

.. toctree::
   :maxdepth: 2
   :caption: Cookbook

   cookbook/libraries-with-smart-constructors
   cookbook/protocol-buffers

.. toctree::
   :maxdepth: 2
   :caption: Troubleshooting

   troubleshooting/known-issues-and-limitations
   troubleshooting/automatic-semiautomatic-and-inlined-derivation
   troubleshooting/debugging-macros

.. toctree::
   :maxdepth: 2
   :caption: Migration

   migration/migration-to-0.8

.. toctree::
   :maxdepth: 1
   :caption: Benchmarks

   benchmarks
