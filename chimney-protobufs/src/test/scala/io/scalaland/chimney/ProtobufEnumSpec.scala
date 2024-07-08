package io.scalaland.chimney

// format: off
import io.scalaland.chimney.dsl._
// format: on
import io.scalaland.chimney.examples.pb
import io.scalaland.chimney.fixtures.{addressbook, order}

class ProtobufEnumSpec extends ChimneySpec {

  group("PB enum") {

    test("totally transform from sealed trait hierarchy of case objects") {
      (addressbook.MOBILE: addressbook.PhoneType)
        .transformInto[pb.addressbook.PhoneType] ==> pb.addressbook.PhoneType.MOBILE
      (addressbook.HOME: addressbook.PhoneType)
        .transformInto[pb.addressbook.PhoneType] ==> pb.addressbook.PhoneType.HOME
      (addressbook.WORK: addressbook.PhoneType)
        .transformInto[pb.addressbook.PhoneType] ==> pb.addressbook.PhoneType.WORK
    }

    test("partially transform into sealed trait hierarchy of case objects (with manual handling of unrecognized)") {
      (pb.addressbook.PhoneType.MOBILE: pb.addressbook.PhoneType)
        .intoPartial[addressbook.PhoneType]
        .withEnumCaseHandledPartial[pb.addressbook.PhoneType.Unrecognized](_ => partial.Result.fromEmpty)
        .transform
        .asOption ==> Some(addressbook.MOBILE)
      (pb.addressbook.PhoneType.HOME: pb.addressbook.PhoneType)
        .intoPartial[addressbook.PhoneType]
        .withEnumCaseHandledPartial[pb.addressbook.PhoneType.Unrecognized](_ => partial.Result.fromEmpty)
        .transform
        .asOption ==> Some(addressbook.HOME)
      (pb.addressbook.PhoneType.WORK: pb.addressbook.PhoneType)
        .intoPartial[addressbook.PhoneType]
        .withEnumCaseHandledPartial[pb.addressbook.PhoneType.Unrecognized](_ => partial.Result.fromEmpty)
        .transform
        .asOption ==> Some(addressbook.WORK)
      (pb.addressbook.PhoneType.Unrecognized(0): pb.addressbook.PhoneType)
        .intoPartial[addressbook.PhoneType]
        .withEnumCaseHandledPartial[pb.addressbook.PhoneType.Unrecognized](_ => partial.Result.fromEmpty)
        .transform
        .asOption ==> None
    }

    test("partially transform into sealed trait hierarchy of case objects (handling Unrecognized with implicit)") {
      // format: off
      import protobufs._
      // format: on

      (pb.addressbook.PhoneType.MOBILE: pb.addressbook.PhoneType)
        .transformIntoPartial[addressbook.PhoneType]
        .asOption ==> Some(addressbook.MOBILE)
      (pb.addressbook.PhoneType.HOME: pb.addressbook.PhoneType)
        .transformIntoPartial[addressbook.PhoneType]
        .asOption ==> Some(addressbook.HOME)
      (pb.addressbook.PhoneType.WORK: pb.addressbook.PhoneType)
        .transformIntoPartial[addressbook.PhoneType]
        .asOption ==> Some(addressbook.WORK)
      (pb.addressbook.PhoneType.Unrecognized(0): pb.addressbook.PhoneType)
        .transformIntoPartial[addressbook.PhoneType]
        .asOption ==> None
    }
  }
}
