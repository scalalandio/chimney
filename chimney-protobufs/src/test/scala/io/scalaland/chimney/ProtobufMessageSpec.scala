package io.scalaland.chimney

// format: off
import io.scalaland.chimney.dsl._
// format: on
import io.scalaland.chimney.examples.pb
import io.scalaland.chimney.fixtures.{addressbook, order}

class ProtobufMessageSpec extends ChimneySpec {

  group("transform bigger case classes") {

    test("PhoneNumber") {

      addressbook
        .PhoneNumber("1234567", addressbook.HOME)
        .transformInto[pb.addressbook.PhoneNumber] ==>
        pb.addressbook.PhoneNumber("1234567", pb.addressbook.PhoneType.HOME)
    }

    test("Person") {

      addressbook
        .Person(
          addressbook.PersonName("John"),
          addressbook.PersonId(123),
          addressbook.Email("john@example.com"),
          List(
            addressbook.PhoneNumber("1234567", addressbook.HOME),
            addressbook.PhoneNumber("77332233", addressbook.WORK),
            addressbook.PhoneNumber("88776655", addressbook.MOBILE)
          )
        )
        .transformInto[pb.addressbook.Person] ==>
        pb.addressbook.Person(
          "John",
          123,
          "john@example.com",
          Seq(
            pb.addressbook.PhoneNumber("1234567", pb.addressbook.PhoneType.HOME),
            pb.addressbook.PhoneNumber("77332233", pb.addressbook.PhoneType.WORK),
            pb.addressbook.PhoneNumber("88776655", pb.addressbook.PhoneType.MOBILE)
          )
        )
    }

    test("AddressBook") {

      addressbook
        .AddressBook(
          List(
            addressbook.Person(
              addressbook.PersonName("John"),
              addressbook.PersonId(123),
              addressbook.Email("john@example.com"),
              List(
                addressbook.PhoneNumber("1234567", addressbook.HOME),
                addressbook.PhoneNumber("77332233", addressbook.WORK),
                addressbook.PhoneNumber("88776655", addressbook.MOBILE)
              )
            ),
            addressbook.Person(
              addressbook.PersonName("Susan"),
              addressbook.PersonId(321),
              addressbook.Email("susan@example.com"),
              List(addressbook.PhoneNumber("200300400", addressbook.MOBILE))
            )
          )
        )
        .transformInto[pb.addressbook.AddressBook] ==>
        pb.addressbook.AddressBook(
          Seq(
            pb.addressbook.Person(
              "John",
              123,
              "john@example.com",
              Seq(
                pb.addressbook.PhoneNumber("1234567", pb.addressbook.PhoneType.HOME),
                pb.addressbook.PhoneNumber("77332233", pb.addressbook.PhoneType.WORK),
                pb.addressbook.PhoneNumber("88776655", pb.addressbook.PhoneType.MOBILE)
              )
            ),
            pb.addressbook.Person(
              "Susan",
              321,
              "susan@example.com",
              Seq(pb.addressbook.PhoneNumber("200300400", pb.addressbook.PhoneType.MOBILE))
            )
          )
        )
    }

    group("Order") {

      group("success case") {

        val domainOrder =
          order.Order(
            List(order.OrderLine(order.Item(123, "foo"), 3), order.OrderLine(order.Item(321, "bar"), 1)),
            order.Customer(123, "John", "Beer", order.Address("street", 1137, "city"))
          )
        val pbOrder = pb.order.Order(
          Seq(
            pb.order.OrderLine(Option(pb.order.Item(123, "foo")), 3),
            pb.order.OrderLine(Option(pb.order.Item(321, "bar")), 1)
          ),
          Option(pb.order.Customer(123, "John", "Beer", Option(pb.order.Address("street", 1137, "city"))))
        )

        test("using total transformers") {

          domainOrder.into[pb.order.Order].transform ==> pbOrder
        }

        test("using partial transformers") {
          pbOrder.into[order.Order].partial.transform ==> partial.Result.fromValue(domainOrder)
        }
      }

      group("failure case") {

        val pbFailureOrder = pb.order.Order(
          Seq(
            pb.order.OrderLine(Option(pb.order.Item(123, "foo")), 3),
            pb.order.OrderLine(None, 1)
          ),
          Option(pb.order.Customer(123, "John", "Beer", None))
        )

        test("using partial transformers") {
          val result = pbFailureOrder.into[order.Order].partial.transform

          result.asOption ==> None
          result.asErrorPathMessageStrings ==> Iterable(
            "lines(1).item" -> "empty value",
            "customer.address" -> "empty value"
          )
        }
      }
    }
  }
}
