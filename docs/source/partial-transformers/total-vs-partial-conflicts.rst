.. _total-vs-partial-conflicts:

Total vs partial conflicts
==========================

Under normal circumstances when fields or sealed hierarchy subtypes should be converted, Chimney should be able
to generate code on its own. If it cannot you could provide it with derivation settings or implicit transformer handling
problematic fields/subtypes.

It can use total transformers

.. code-block:: scala

  import io.scalaland.chimney._
  import io.scalaland.chimney.dsl._

  case class UserAPI(credits: String)
  case class User(credits: Int)

  implicit val unsafeStringToInt: Transformer[String, Int] = _.toInt

  UserAPI("10").transformIntoPartial[User].asOption // Some(User(10))

or partial transformer

.. code-block:: scala

  import io.scalaland.chimney._
  import io.scalaland.chimney.dsl._

  case class UserAPI(credits: String)
  case class User(credits: Int)

  implicit val parseStringToInt: PartialTransformer[String, Int] =
    PartialTransformer[String, Int](str => partial.Result.fromCatching(str.toInt).map(_ * 2))

  UserAPI("10").transformIntoPartial[User].asOption // Some(User(20))

But which implicit should be taken in this situation?

.. code-block:: scala

  import io.scalaland.chimney._
  import io.scalaland.chimney.dsl._

  case class UserAPI(credits: String)
  case class User(credits: Int)

  implicit val unsafeStringToInt: Transformer[String, Int] = _.toInt

  implicit val parseStringToInt: PartialTransformer[String, Int] =
    PartialTransformer[String, Int](str => partial.Result.fromCatching(str.toInt).map(_ * 2))

  UserAPI("10").transformIntoPartial[User].asOption
  // error: Ambiguous implicits while resolving Chimney recursive transformation:
  //
  //        PartialTransformer[String, Int]: parseStringToInt
  //        Transformer[String, Int]: unsafeStringToInt

To avoid the ambiguity, Chimney would fail the derivation in such case and expect you to tell it, which transformer it
should prefer: total or partial:

.. code-block:: scala

  import io.scalaland.chimney._
  import io.scalaland.chimney.dsl._

  case class UserAPI(credits: String)
  case class User(credits: Int)

  implicit val unsafeStringToInt: Transformer[String, Int] = _.toInt

  implicit val parseStringToInt: PartialTransformer[String, Int] =
    PartialTransformer[String, Int](str => partial.Result.fromCatching(str.toInt).map(_ * 2))

  UserAPI("10").intoPartial[User]
    .enableImplicitConflictResolution(PreferTotalTransformer)
    .transform.asOption // Some(User(10))

  UserAPI("10").intoPartial[User]
    .enableImplicitConflictResolution(PreferPartialTransformer)
    .transform.asOption // Some(User(20))
