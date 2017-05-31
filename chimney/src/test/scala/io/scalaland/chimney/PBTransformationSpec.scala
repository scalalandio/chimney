package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}
import io.scalaland.chimney.examples.addressbook
import shapeless.test._

class PBTransformationSpec extends WordSpec with MustMatchers {

  import dsl._

  "Domain to Protobuf" should {

    "transform value classes between their primitive representations" in {

      addressbook.PersonName("John").transformInto[String] mustBe "John"
      addressbook.PersonId(5).transformInto[Int] mustBe 5
      addressbook.Email("john@example.com").transformInto[String] mustBe "john@example.com"
    }

    "not compile if target type is wrong for value class" in {

      illTyped(""" addressbook.PersonName("John").transformInto[Int] """)
      illTyped(""" addressbook.PersonId(5).transformInto[String] """)
      illTyped(""" addressbook.Email("john@example.com").transformInto[Float] """)
    }

    "transform enum represented as sealed trait hierarchy" in {

      (addressbook.MOBILE: addressbook.PhoneType)
        .transformInto[examples.pb.addressbook.PhoneType] mustBe
        examples.pb.addressbook.PhoneType.MOBILE

      (addressbook.HOME: addressbook.PhoneType)
        .transformInto[examples.pb.addressbook.PhoneType] mustBe
        examples.pb.addressbook.PhoneType.HOME

      (addressbook.WORK: addressbook.PhoneType)
        .transformInto[examples.pb.addressbook.PhoneType] mustBe
        examples.pb.addressbook.PhoneType.WORK
    }

    "transform bigger case classes" when {

      "PhoneNumber" in {

        addressbook.PhoneNumber("1234567", addressbook.HOME)
          .transformInto[examples.pb.addressbook.Person.PhoneNumber] mustBe
          examples.pb.addressbook.Person.PhoneNumber("1234567", examples.pb.addressbook.PhoneType.HOME)


      }

    }

  }

}
