# Chimney

Battle tested Scala library for boilerplate-free data transformations.

That does it mean? Imagine you'd have to convert between this Protobuf definition:

```scala
case class UserPB(
    name: String,
    addresses: Seq[AddressPB],
    recovery: Option[RecoveryMethodPB]
)

case class AddressPB(street: String, city: String)

sealed trait RecoveryMethodPB
object RecoveryMethodPB {
  case class Phone(value: PhonePB) extends RecoveryMethodPB
  case class Email(value: EmailPB) extends RecoveryMethodPB
}

case class PhonePB(number: String)
case class EmailPB(email: String)
```

and this domain model:

```scala
case class User(
    name: Username,
    addresses: List[Address],
    recovery: RecoveryMethod
)
case class Username(name: String) extends AnyVal

case class Address(street: String, city: String)

enum RecoveryMethod:
  case Phone(number: String)
  case Email(email: String)
```

Can you imagine all the code you'd have to write? And the necessity to update carefully when the model changes? The
silly mistakes with wrong field you inevitably make while copy-pasting a lot of repetitive, boring and dumb code?

From now on, forget about it. Encoding domain object with an infallible transformation, like a total function?

```scala
import io.scalaland.chimney.dsl.*

User(
  Username("John"),
  List(Address("Paper St", "Somewhere")),
  RecoveryMethod.Email("john@example.com")
).transformInto[UserPB]
// UserPB(
//   "John",
//   Seq(AddressPB("Paper St", "Somewhere")),
//   Some(RecoveryMethodPB.Email(EmailPB("john@example.com")))
// )
```

Done! Decoding protobuf into domain object with a fallible transformation, like a partial-function but safer?

```scala
UserPB(
  "John",
  Seq(AddressPB("Paper St", "Somewhere")),
  Option(RecoveryMethodPB.Email(EmailPB("john@example.com")))
).transformIntoPartial[User].asEither
// Right(User(
//   Username("John"),
//   List(Address("Paper St", "Somewhere")),
//   RecoveryMethod.Email("john@example.com")
// ))

UserPB(
  "John",
  Seq(AddressPB("Paper St", "Somewhere")),
  None
).transformIntoPartial[User].asEither
  .left.map(_.asErrorPathMessages)
// Left(List("recovery" -> EmptyValue))
```

Also done! And if a field cannot be converted, you'll get the path to problematic value!

Now, go to the [quick start section](/quickstart/) to learn how to get Chimney and move on
to [supported transformations section](/supported-transformations/) to learn about a plenty of transformations
supported out of the box and even more enabled with easy customizations.
