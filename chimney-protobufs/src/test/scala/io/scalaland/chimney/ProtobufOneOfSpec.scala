package io.scalaland.chimney

// format: off
import io.scalaland.chimney.dsl._
// format: on
import io.scalaland.chimney.examples.pb
import io.scalaland.chimney.fixtures.{addressbook, order}

class ProtobufOneOfSpec extends ChimneySpec {

  group("transformer sealed traits generated from oneof") {

    test("AddressBookType (oneof value - sealed contains single-value wrappers around actual products)") {
      val domainType: addressbook.AddressBookType = addressbook.AddressBookType.Private("test")
      val pbType: pb.addressbook.AddressBookType =
        pb.addressbook.AddressBookType.of(
          pb.addressbook.AddressBookType.Value.Private(pb.addressbook.AddressBookType.Private.of("test"))
        )

      domainType.into[pb.addressbook.AddressBookType.Value].transform ==> pbType.value

      pbType.value
        .intoPartial[addressbook.AddressBookType]
        .withCoproductInstancePartial[pb.addressbook.AddressBookType.Value.Empty.type](_ => partial.Result.fromEmpty)
        .transform
        .asOption ==> Some(domainType)
      locally {
        // format: off
        import protobufs._
        // format: on
        pbType.value.intoPartial[addressbook.AddressBookType].transform.asOption ==> Some(domainType)
      }
    }

    test("CustomerStatus (oneof sealed_value - flat representation with additional Empty vase)") {
      val domainStatus: order.CustomerStatus = order.CustomerStatus.CustomerRegistered
      val pbStatus: pb.order.CustomerStatus = pb.order.CustomerRegistered()

      domainStatus.into[pb.order.CustomerStatus].transform ==> pbStatus

      pbStatus
        .intoPartial[order.CustomerStatus]
        .withCoproductInstancePartial[pb.order.CustomerStatus.Empty.type](_ => partial.Result.fromEmpty)
        .withCoproductInstance[pb.order.CustomerStatus.NonEmpty](_.transformInto[order.CustomerStatus])
        .transform
        .asOption ==> Some(domainStatus)
    }

    test("PaymentStatus (oneof sealed_value_optional - flat representation wrapped in Option)") {
      val domainStatus: Option[order.PaymentStatus] = Option(order.PaymentStatus.PaymentRequested)
      val pbStatus: Option[pb.order.PaymentStatus] = Option(pb.order.PaymentRequested())

      domainStatus.into[Option[pb.order.PaymentStatus]].transform ==> pbStatus
      pbStatus.into[Option[order.PaymentStatus]].transform ==> domainStatus
    }
  }
}
