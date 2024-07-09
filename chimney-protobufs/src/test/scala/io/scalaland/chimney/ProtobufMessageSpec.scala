package io.scalaland.chimney

// format: off
import io.scalaland.chimney.dsl._
// format: on
import io.scalaland.chimney.examples.pb
import io.scalaland.chimney.fixtures.{addressbook, order, user}

class ProtobufMessageSpec extends ChimneySpec {

  // enums decoding is handled in another suite
  implicit val decodePhoneType: PartialTransformer[pb.addressbook.PhoneType, addressbook.PhoneType] =
    PartialTransformer
      .define[pb.addressbook.PhoneType, addressbook.PhoneType]
      .withEnumCaseHandledPartial[pb.addressbook.PhoneType.Unrecognized](_ => partial.Result.fromEmpty)
      .buildTransformer

  group("messages compiled with preserve_unknown_fields: false") {

    val pbPhone: pb.addressbook.PhoneNumber = pb.addressbook.PhoneNumber("1234567", pb.addressbook.PhoneType.HOME)
    val pbPerson: pb.addressbook.Person = pb.addressbook.Person(
      "John",
      123,
      "john@example.com",
      Seq(
        pbPhone,
        pb.addressbook.PhoneNumber("77332233", pb.addressbook.PhoneType.WORK),
        pb.addressbook.PhoneNumber("88776655", pb.addressbook.PhoneType.MOBILE)
      )
    )
    val pbAddressBook: pb.addressbook.AddressBook = pb.addressbook.AddressBook(
      Seq(
        pbPerson,
        pb.addressbook.Person(
          "Susan",
          321,
          "susan@example.com",
          Seq(pb.addressbook.PhoneNumber("200300400", pb.addressbook.PhoneType.MOBILE))
        )
      )
    )
    val pbOrder: pb.order.Order = pb.order.Order(
      Seq(
        pb.order.OrderLine(Option(pb.order.Item(123, "foo")), 3),
        pb.order.OrderLine(Option(pb.order.Item(321, "bar")), 1)
      ),
      Option(pb.order.Customer(123, "John", "Beer", Option(pb.order.Address("street", 1137, "city"))))
    )
    val pbOrderInvalid = pb.order.Order(
      Seq(
        pb.order.OrderLine(Option(pb.order.Item(123, "foo")), 3),
        pb.order.OrderLine(None, 1)
      ),
      Option(pb.order.Customer(123, "John", "Beer", None))
    )

    val domainPhone: addressbook.PhoneNumber = addressbook.PhoneNumber("1234567", addressbook.HOME)
    val domainPerson: addressbook.Person = addressbook.Person(
      addressbook.PersonName("John"),
      addressbook.PersonId(123),
      addressbook.Email("john@example.com"),
      List(
        domainPhone,
        addressbook.PhoneNumber("77332233", addressbook.WORK),
        addressbook.PhoneNumber("88776655", addressbook.MOBILE)
      )
    )
    val domainAddressBook: addressbook.AddressBook = addressbook
      .AddressBook(
        List(
          domainPerson,
          addressbook.Person(
            addressbook.PersonName("Susan"),
            addressbook.PersonId(321),
            addressbook.Email("susan@example.com"),
            List(addressbook.PhoneNumber("200300400", addressbook.MOBILE))
          )
        )
      )
    val domainOrder: order.Order = order.Order(
      List(order.OrderLine(order.Item(123, "foo"), 3), order.OrderLine(order.Item(321, "bar"), 1)),
      order.Customer(123, "John", "Beer", order.Address("street", 1137, "city"))
    )

    test("totally transform from cases classes") {
      domainPhone.transformInto[pb.addressbook.PhoneNumber] ==> pbPhone
      domainPerson.transformInto[pb.addressbook.Person] ==> pbPerson
      domainAddressBook.transformInto[pb.addressbook.AddressBook] ==> pbAddressBook
      domainOrder.into[pb.order.Order].transform ==> pbOrder
    }

    test("partially transform into case classes unwrapping present optional values") {
      pbPhone.transformIntoPartial[addressbook.PhoneNumber].asOption ==> Some(domainPhone)
      pbPerson.transformIntoPartial[addressbook.Person].asOption ==> Some(domainPerson)
      pbAddressBook.transformIntoPartial[addressbook.AddressBook].asOption ==> Some(domainAddressBook)
      pbOrder.transformIntoPartial[order.Order].asOption ==> Some(domainOrder)
    }

    test("partially transform into case classes failing absent optional values with Empty") {
      val result = pbOrderInvalid.transformIntoPartial[order.Order]
      result.asOption ==> None
      result.asErrorPathMessageStrings ==> Iterable(
        "lines(1).item" -> "empty value",
        "customer.address" -> "empty value"
      )
    }
  }

  group("messages compiled with preserve_unknown_fields: true") {

    val pbUser = pb.user.User(
      "Susan",
      321,
      "susan@example.com",
      Seq(pb.addressbook.PhoneNumber("200300400", pb.addressbook.PhoneType.MOBILE))
    )
    val domainUser = user.User(
      addressbook.PersonName("Susan"),
      addressbook.PersonId(321),
      addressbook.Email("susan@example.com"),
      List(addressbook.PhoneNumber("200300400", addressbook.MOBILE))
    )

    test("totally transform from cases classes (enabling default values for UnknownFieldSet)") {
      domainUser.into[pb.user.User].enableDefaultValues.transform ==> pbUser
      domainUser.into[pb.user.User].enableDefaultValueOfType[scalapb.UnknownFieldSet].transform ==> pbUser
    }

    test("partially transform into cases classes") {
      pbUser.transformIntoPartial[user.User].asOption ==> Some(domainUser)
    }
  }
}
