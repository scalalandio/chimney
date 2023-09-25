package io.scalaland.chimney

// format: off
import io.scalaland.chimney.dsl._
// format: on
import io.scalaland.chimney.examples.pb
import io.scalaland.chimney.fixtures.{addressbook, order}

class ProtobufEnumSpec extends ChimneySpec {

  test("transform enum represented as sealed trait hierarchy") {

    (addressbook.MOBILE: addressbook.PhoneType)
      .transformInto[pb.addressbook.PhoneType] ==> pb.addressbook.PhoneType.MOBILE
    (addressbook.HOME: addressbook.PhoneType).transformInto[pb.addressbook.PhoneType] ==> pb.addressbook.PhoneType.HOME
    (addressbook.WORK: addressbook.PhoneType).transformInto[pb.addressbook.PhoneType] ==> pb.addressbook.PhoneType.WORK
  }
}
