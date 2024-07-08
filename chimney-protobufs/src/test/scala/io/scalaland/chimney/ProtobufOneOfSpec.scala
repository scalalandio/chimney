package io.scalaland.chimney

// format: off
import io.scalaland.chimney.dsl._
// format: on
import io.scalaland.chimney.examples.pb
import io.scalaland.chimney.fixtures.{addressbook, order}

class ProtobufOneOfSpec extends ChimneySpec {

  group("oneof value (represented with wrapped sealed hierarchy)") {
    val domainType: addressbook.AddressBookType = addressbook.AddressBookType.Private("test")
    val pbType: pb.addressbook.AddressBookType =
      pb.addressbook.AddressBookType.of(
        pb.addressbook.AddressBookType.Value.Private(pb.addressbook.AddressBookType.Private.of("test"))
      )

    test("totally transform from sealed hierarchy") {
      domainType.into[pb.addressbook.AddressBookType.Value].transform ==> pbType.value
    }

    test("partially transform into sealed hierarchy (manually handling Value.Empty)") {
      pbType.value
        .intoPartial[addressbook.AddressBookType]
        .withSealedSubtypeHandledPartial[pb.addressbook.AddressBookType.Value.Empty.type](_ => partial.Result.fromEmpty)
        .transform
        .asOption ==> Some(domainType)
    }

    test("partially transform into sealed hierarchy (handling Value.Empty.type with implicit)") {
      // format: off
      import protobufs._
      // format: on
      pbType.value.intoPartial[addressbook.AddressBookType].transform.asOption ==> Some(domainType)
    }
  }

  group("oneof sealed_value (represented with flat sealed hierarchy with additional Empty case)") {
    val domainStatus: order.CustomerStatus = order.CustomerStatus.CustomerRegistered
    val pbStatus: pb.order.CustomerStatus = pb.order.CustomerRegistered()

    test("totally transform from sealed hierarchy") {
      domainStatus.into[pb.order.CustomerStatus].transform ==> pbStatus
    }

    test("partially transform into sealed hierarchy (manually handling Empty and NonEmpty subtypes)") {
      pbStatus
        .intoPartial[order.CustomerStatus]
        .withSealedSubtypeHandledPartial[pb.order.CustomerStatus.Empty.type](_ => partial.Result.fromEmpty)
        .withSealedSubtypeHandled[pb.order.CustomerStatus.NonEmpty](_.transformInto[order.CustomerStatus])
        .transform
        .asOption ==> Some(domainStatus)
    }
  }

  group("oneof sealed_value_optional (represented with flat sealed hierarchy wrapped in Option)") {
    val domainStatus: Option[order.PaymentStatus] = Option(order.PaymentStatus.PaymentRequested)
    val pbStatus: Option[pb.order.PaymentStatus] = Option(pb.order.PaymentRequested())

    test("totally transform from sealed hierarchy") {
      domainStatus.into[Option[pb.order.PaymentStatus]].transform ==> pbStatus
    }

    test("totally transform into sealed hierarchy") {
      pbStatus.into[Option[order.PaymentStatus]].transform ==> domainStatus
    }
  }
}
