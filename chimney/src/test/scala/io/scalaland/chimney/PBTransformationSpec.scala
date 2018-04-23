package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}
import io.scalaland.chimney.examples.addressbook
import io.scalaland.chimney.examples.pb

class PBTransformationSpec extends WordSpec with MustMatchers {

  import dsl._

  "Domain to Protobuf" should {

    "transform value classes between their primitive representations" in {

      addressbook.PersonName("John").transformInto[String] mustBe "John"
      addressbook.PersonId(5).transformInto[Int] mustBe 5
      addressbook.Email("john@example.com").transformInto[String] mustBe "john@example.com"
    }

    "not compile if target type is wrong for value class" in {

      assertTypeError(""" addressbook.PersonName("John").transformInto[Int] """)
      assertTypeError(""" addressbook.PersonId(5).transformInto[String] """)
      assertTypeError(""" addressbook.Email("john@example.com").transformInto[Float] """)
    }

    "transform enum represented as sealed trait hierarchy" in {

      (addressbook.MOBILE: addressbook.PhoneType)
        .transformInto[pb.addressbook.PhoneType] mustBe
        pb.addressbook.PhoneType.MOBILE

      (addressbook.HOME: addressbook.PhoneType)
        .transformInto[pb.addressbook.PhoneType] mustBe
        pb.addressbook.PhoneType.HOME

      (addressbook.WORK: addressbook.PhoneType)
        .transformInto[pb.addressbook.PhoneType] mustBe
        pb.addressbook.PhoneType.WORK
    }

    "transform bigger case classes" when {

      "PhoneNumber" in {

        addressbook
          .PhoneNumber("1234567", addressbook.HOME)
          .transformInto[pb.addressbook.PhoneNumber] mustBe
          pb.addressbook.PhoneNumber("1234567", pb.addressbook.PhoneType.HOME)
      }

      "Person" in {

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
          .transformInto[pb.addressbook.Person] mustBe
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

      "AddressBook" in {

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
          .transformInto[pb.addressbook.AddressBook] mustBe
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
    }
  }

}
