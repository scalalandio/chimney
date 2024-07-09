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

    test("totally transform wrapped value from sealed hierarchy") {
      domainType.transformInto[pb.addressbook.AddressBookType.Value] ==> pbType.value
    }

    test("totally transform wrapper value from sealed hierarchy (manually enabling non-AnyVal wrappers)") {
      domainType.into[pb.addressbook.AddressBookType].enableNonAnyValWrappers.transform ==> pbType

      locally {
        implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers

        domainType.transformInto[pb.addressbook.AddressBookType] ==> pbType
      }
    }

    test("partially transform wrapped value into sealed hierarchy (manually handling Value.Empty)") {
      pbType.value
        .intoPartial[addressbook.AddressBookType]
        .withSealedSubtypeHandledPartial[pb.addressbook.AddressBookType.Value.Empty.type](_ => partial.Result.fromEmpty)
        .transform
        .asOption ==> Some(domainType)
    }

    test("partially transform wrapped into sealed hierarchy (handling Value.Empty.type with implicit)") {
      // format: off
      import protobufs._
      // format: on

      pbType.value.transformIntoPartial[addressbook.AddressBookType].asOption ==> Some(domainType)
    }

    test(
      "partially transform wrapper into sealed hierarchy (handling Value.Empty.type with implicit, and enabling non-AnyVal wrappers)"
    ) {
      // format: off
      import protobufs._
      // format: on

      pbType.intoPartial[addressbook.AddressBookType].enableNonAnyValWrappers.transform.asOption ==> Some(domainType)

      locally {
        implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers
        pbType.transformIntoPartial[addressbook.AddressBookType].asOption ==> Some(domainType)
      }
    }
  }

  group("oneof sealed_value (represented with flat sealed hierarchy with additional Empty case)") {
    val domainStatus: order.CustomerStatus = order.CustomerStatus.CustomerRegistered
    val pbStatus: pb.order.CustomerStatus = pb.order.CustomerRegistered()

    test("totally transform from sealed hierarchy") {
      domainStatus.transformInto[pb.order.CustomerStatus] ==> pbStatus
    }

    test("partially transform into sealed hierarchy (manually handling Empty and NonEmpty subtypes)") {
      pbStatus
        .intoPartial[order.CustomerStatus]
        .withSealedSubtypeHandledPartial[pb.order.CustomerStatus.Empty.type](_ => partial.Result.fromEmpty)
        .withSealedSubtypeHandled[pb.order.CustomerStatus.NonEmpty](_.transformInto[order.CustomerStatus])
        .transform
        .asOption ==> Some(domainStatus)
    }

    test("partially transform into sealed hierarchy (handling Value.Empty.type with implicit)") {
      // format: off
      //import protobufs._
      // format: on

      // TODO: figure that out: on Scala 3 macro cannot figure out this implicit while on Scala 2 it can :/
      implicit val f: PartialTransformer[pb.order.CustomerStatus.Empty.type, order.CustomerStatus] =
        protobufs.partialTransformerFromEmptySealedOneOfInstance

      pbStatus.transformIntoPartial[order.CustomerStatus].asOption ==> Some(domainStatus)
    }
  }

  group("oneof sealed_value_optional (represented with flat sealed hierarchy wrapped in Option)") {
    val domainStatus: Option[order.PaymentStatus] = Option(order.PaymentStatus.PaymentRequested)
    val pbStatus: Option[pb.order.PaymentStatus] = Option(pb.order.PaymentRequested())

    test("totally transform from sealed hierarchy") {
      domainStatus.transformInto[Option[pb.order.PaymentStatus]] ==> pbStatus
    }

    test("totally transform into sealed hierarchy") {
      pbStatus.transformInto[Option[order.PaymentStatus]] ==> domainStatus
    }
  }
}
