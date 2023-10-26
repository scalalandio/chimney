<h1 style="margin-bottom:0">Chimney</h1>
<h2 style="margin-top:0">The battle-tested Scala library for data transformations</h2>

**Removing boilerplate since 2017.**

<br/>

What does it mean? Imagine you'd have to convert between this Protobuf-like definitions:

!!! example

    ```scala
    case class UserDTO(
        name: String, // 1. primitive
        addresses: Seq[AddressDTO], // 2. Seq collection
        recovery: Option[RecoveryMethodDTO] // 3. Option type
    )
    
    case class AddressDTO(street: String, city: String)
    
    // 4. ADT is not flat - each oneOf message created 2 case classes
    sealed trait RecoveryMethodDTO
    object RecoveryMethodDTO {
      case class Phone(value: PhoneDTO) extends RecoveryMethodDTO
      case class Email(value: EmailDTO) extends RecoveryMethodDTO
    }
    
    case class PhoneDTO(number: String)
    case class EmailDTO(email: String)
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

  - you'd have to wrap and unwrap `AnyVal`
  - you'd have to convert a collection
  - in transformation in one way you'd have to wrap with `Option`, and on the way back handle `None`
  - in one transformation you'd have to manually flatten ADT, and on the way back you have'd to unflatten it

Can you imagine all the code you'd have to write? For now! And the necessity to carefully update when the model changes?
The silly mistakes with using the wrong field you'll inevitably make while copy-pasting a lot of repetitive, boring
and dumb code?

From now on, forget about it! Encoding domain object with an infallible transformation, like a total function?

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._
    
    User(
      Username("John"),
      List(Address("Paper St", "Somewhere")),
      RecoveryMethod.Email("john@example.com")
    ).transformInto[UserDTO]
    // UserDTO(
    //   "John",
    //   Seq(AddressDTO("Paper St", "Somewhere")),
    //   Some(RecoveryMethodDTO.Email(EmailDTO("john@example.com")))
    // )
    ```

??? example "Curious about the generated code?"

    ```scala
    // macro outputs code like this (reformatted a bit for readability):
    final class $anon() extends Transformer[User, UserDTO] {
      def transform(src: User): UserDTO = {
        val user: User = src
        new UserDTO(
          user.name.name,
          user.addresses.iterator.map[AddressDTO](((param: Address) => {
            val `user.addresses`: Address = param
            new AddressDTO(`user.addresses`.street, `user.addresses`.city)
          })).to[Seq[AddressDTO]](Seq.iterableFactory[AddressDTO]),
          Option[RecoveryMethod](user.recovery).map[RecoveryMethodDTO](((`param₂`: RecoveryMethod) => {
            val recoverymethod: RecoveryMethod = `param₂`
            (recoverymethod: RecoveryMethod) match {
              case phone: RecoveryMethod.Phone =>
                new RecoveryMethodDTO.Phone(new PhoneDTO(phone.number))
              case email: RecoveryMethod.Email =>
                new RecoveryMethodDTO.Email(new EmailDTO(email.email))
            }
          }))
        )
      }
    }
 
    (new $anon(): Transformer[User, UserDTO])
    ```

Done! Decoding Protobuf into domain object with a fallible transformation, like a partial-function but safer?

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ git.tag or local.tag }}
    import io.scalaland.chimney.dsl._

    UserDTO(
      "John",
      Seq(AddressDTO("Paper St", "Somewhere")),
      Option(RecoveryMethodDTO.Email(EmailDTO("john@example.com")))
    )
      .transformIntoPartial[User].asEither
      .left.map(_.asErrorPathMessages)
    // Right(User(
    //   Username("John"),
    //   List(Address("Paper St", "Somewhere")),
    //   RecoveryMethod.Email("john@example.com")
    // ))
    
    UserDTO(
      "John",
      Seq(AddressDTO("Paper St", "Somewhere")),
      None
    )
      .transformIntoPartial[User].asEither
      .left.map(_.asErrorPathMessages)
    // Left(List("recovery" -> EmptyValue))
    ```

??? example "Curious about the generated code?"

    ```scala
    // macro outputs code like this (reformatted a bit for readability): 
    final class $anon() extends PartialTransformer[UserDTO, User] {
      def transform(src: UserDTO, failFast: Boolean): partial.Result[User] = {
        val userdto: UserDTO = src
        userdto.recovery.map[partial.Result[RecoveryMethod]](((param: RecoveryMethodDTO) => {
          val recoverymethoddto: RecoveryMethodDTO = param
          partial.Result.Value[RecoveryMethod]((recoverymethoddto: RecoveryMethodDTO) match {
            case phone: RecoveryMethodDTO.Phone =>
              new RecoveryMethod.Phone(phone.value.number)
            case email: RecoveryMethodDTO.Email =>
              new RecoveryMethod.Email(email.value.email)
          })
        }))
          .getOrElse[partial.Result[RecoveryMethod]](partial.Result.fromEmpty[RecoveryMethod])
          .prependErrorPath(partial.PathElement.Accessor("recovery"))
          .map[User](((`param₂`: RecoveryMethod) => {
            val recoverymethod: RecoveryMethod = `param₂`
            new User(
              new Username(userdto.name),
              userdto.addresses.iterator.map[Address](((`param₃`: AddressDTO) => {
                val `userdto.addresses`: AddressDTO = `param₃`
                new Address(`userdto.addresses`.street, `userdto.addresses`.city)
              })).to[List[Address]](List.iterableFactory[Address]),
              recoverymethod
            )
          }))
      }
    }
 
    (new $anon(): PartialTransformer[UserDTO, User])
    ```

Also done! And if a field cannot be converted, you'll get the path to the problematic value!

Now, visit the [quick start section](quickstart.md) to learn how to get Chimney and the move
to the [supported transformations section](supported-transformations.md) to learn about a plethora of transformations
supported out of the box and even more enabled with easy customizations!

!!! tip

    If you have any questions don't forget to look at [cookbook](cookbook.md) for new usage
    ideas, [troubleshooting](troubleshooting.md) for solving problems and
    our [GitHub discussions](https://github.com/scalalandio/chimney/discussions) page! 
