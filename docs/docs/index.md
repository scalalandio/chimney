# Chimney

Battle tested library for boilerplate-free data transformations in Scala.

What does it mean? Imagine you'd have to convert between this Protobuf definitions:

!!! example

    ```scala
    case class UserPB(
        name: String, // 1. primitive
        addresses: Seq[AddressPB], // 2. Seq collection
        recovery: Option[RecoveryMethodPB] // 3. Option type
    )
    
    case class AddressPB(street: String, city: String)
    
    // 4. ADT is not flat - each oneOf message 2 case classes
    sealed trait RecoveryMethodPB
    object RecoveryMethodPB {
      case class Phone(value: PhonePB) extends RecoveryMethodPB
      case class Email(value: EmailPB) extends RecoveryMethodPB
    }
    
    case class PhonePB(number: String)
    case class EmailPB(email: String)
    ```

and this domain model:

!!! example

    ```scala
    case class User(
        name: Username, // 1. value class
        addresses: List[Address], // 2. List collection
        recovery: RecoveryMethod // 3. non-Option type
    )
    case class Username(name: String) extends AnyVal
    
    case class Address(street: String, city: String)
    
    // 4. flat enum
    enum RecoveryMethod:
      case Phone(number: String)
      case Email(email: String)
    ```

Can you imagine all the code you'd have to write? And the necessity to update carefully when the model changes? The
silly mistakes with wrong field you inevitably make while copy-pasting a lot of repetitive, boring and dumb code?

From now on, forget about it. Encoding domain object with an infallible transformation, like a total function?

!!! example

    ```scala
    //> using dep "io.scalaland::chimney:{{ git.tag or scm.latest }}"
    import io.scalaland.chimney.dsl._
    
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

!!! example

    ```scala
    //> using dep "io.scalaland::chimney:{{ git.tag or scm.latest }}"
    import io.scalaland.chimney.dsl._

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

Also done! And if a field cannot be converted, you'll get the path to the problematic value!

Now, visit the [quick start section](quickstart.md) to learn how to get Chimney and the move
to the [supported transformations section](supported-transformations.md) to learn about a plethora of transformations
supported out of the box and even more enabled with easy customizations!
