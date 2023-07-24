Protocol Buffers
================

UnknownFieldSet
---------------

By default ScalaPB would generate in a case class an additional field
``unknownFields: UnknownFieldSet = UnknownFieldSet()``. This field
could be used if you want to somehow log/trace that some extra values -
perhaps from another version of the schema - were passed but your current
version's parser didn't need it.

The automatic conversion into a protobuf with such field can be problematic:

.. code-block:: scala

  object domain {
    case class Address(line1: String, line2: String)
  }
  object protobuf {
    case class Address(
      line1: String = "",
      line2: String = "",
      unknownFields: UnknownFieldSet = UnknownFieldSet()
    )
  }

  domain.Address("a", "b").transformInto[protobuf.Address]
  // error: Chimney can't derive transformation from domain.Address to protobuf.Address
  //
  // protobuf.Address
  //   unknownFields: UnknownFieldSet - no accessor named unknownFields in source type domain.Address
  //
  //
  // Consult https://scalalandio.github.io/chimney for usage examples.

There are 2 ways in which Chimney could handle this issue:

- using default values

  .. code-block:: scala

     domain.Address("a", "b").into[protobuf.Address]
       .enableDefaultValues
       .transform

- manually setting this one field

  .. code-block:: scala

     domain.Address("a", "b").into[protobuf.Address]
       .withFieldConst(_.unknownFields, UnknownFieldSet())
       .transform

However, if you have a control over the ScalaPB process, you could configure it
to simply not generate this field, either by `editing the protobuf <https://scalapb.github.io/docs/customizations#file-level-options>`_:

.. code-block:: protobuf

   option (scalapb.options) = {
     preserve_unknown_fields: false
   };

or adding to `package-scoped options <https://scalapb.github.io/docs/customizations#package-scoped-options>`_.
If the field won't be generated in the first place, there will be no issues
with providing values to it.

oneof fields
------------

``oneof`` is a way in which Protocol Buffers allows using ADTs. The example PB:

.. code-block:: protobuf

  message AddressBookType {
    message Public {}
    message Private {
      string owner = 1;
    }
    oneof value {
      Public public = 1;
      Private private = 2;
    }
  }

would generate scala code similar to:

.. code-block:: scala

  package pb.addressbook

  final case class AddressBookType(
      value: AddressBookType.Value = AddressBookType.Value.Empty
  ) extends scalapb.GeneratedMessage
      with scalapb.lenses.Updatable[AddressBookType] {
    // ...
  }

  object AddressBookType
      extends scalapb.GeneratedMessageCompanion[AddressBookType] {
    sealed trait Value extends scalapb.GeneratedOneof
    object Value {
      case object Empty extends AddressBookType.Value {
        type ValueType = _root_.scala.Nothing
        // ...
      }
      final case class Public(value: AddressBookType.Public)
          extends AddressBookType.Value {
        type ValueType = AddressBookType.Public
        // ...
      }
      @SerialVersionUID(0L)
      final case class Private(value: AddressBookType.Private)
          extends AddressBookType.Value {
        type ValueType = AddressBookType.Private
        // ...
      }
    }
    final case class Public(
    ) extends scalapb.GeneratedMessage
        with scalapb.lenses.Updatable[Public] {
    }

    final case class Private(
        owner: _root_.scala.Predef.String = ""
    ) extends scalapb.GeneratedMessage
        with scalapb.lenses.Updatable[Private] {
      // ...
    }

    // ...
  }

As we see:

- there is an extra ``Value.Empty`` type
- this is not "flat" ``sealed`` hierarchy - ``AddressBookType`` wraps
  sealed hierarchy ``AddressBookType.Value``, where each ``case class``
  wraps the actual message

Meanwhile, we would prefer to extract it to flat:

.. code-block:: scala

  package addressbook

  sealed trait AddressBookType
  object AddressBookType {
    case object Public extends AddressBookType
    case class Private(owner: String) extends AddressBookType
  }

Luckily for us, since 0.8.0 Chimney supports automatic unwrapping of sealed
hierarchy cases.

Encoding (with transformers) is pretty straightforward:

.. code-block:: scala

  val domainType: addressbook.AddressBookType = addressbook.AddressBookType.Private("test")
  val pbType: pb.addressbook.AddressBookType =
    pb.addressbook.AddressBookType.of(
      pb.addressbook.AddressBookType.Value.Private(pb.addressbook.AddressBookType.Private.of("test"))
    )

  domainType.into[pb.addressbook.AddressBookType.Value].transform == pbType.value

Decoding (with partial transformers) requires handling of ``Empty.Value`` type
- we can do it manually:

.. code-block:: scala

  pbType.value
    .intoPartial[addressbook.AddressBookType]
    .withCoproductInstancePartial[pb.addressbook.AddressBookType.Value.Empty.type](_ => partial.Result.fromEmpty)
    .transform
    .asOption == Some(domainType)

or handle all such fields with a single implicit:

.. code-block:: scala

  type IsEmpty = scalapb.GeneratedOneof { type ValueType = Nothing }
  implicit def handleEmptyInstance[From <: IsEmpty, To]: PartialTransformer[From, To] =
    PartialTransformer(_ => partial.Result.fromEmpty)

  pbType.value.intoPartial[addressbook.AddressBookType].transform.asOption == Some(domainType)

sealed_value oneof fields
-------------------------

In case we can edit out protobuf definition, we can arrange the generated code
to be flat ``sealed`` hierarchy. It requires `several conditions defined by ScalaPB <https://scalapb.github.io/docs/sealed-oneofs#sealed-oneof-rules>`_.

.. code-block:: protobuf

  message CustomerStatus {
    oneof sealed_value {
      CustomerRegistered registered = 1;
      CustomerOneTime oneTime = 2;
    }
  }

  message CustomerRegistered {}

  message CustomerOneTime {}

would generate something like:

.. code-block:: scala

  package pb.order

  sealed trait CustomerStatus extends scalapb.GeneratedSealedOneof {
    type MessageType = CustomerStatusMessage
  }

  object CustomerStatus {
    case object Empty extends CustomerStatus

    sealed trait NonEmpty extends CustomerStatus
  }

  final case class CustomerRegistered(
  ) extends scalapb.GeneratedMessage
      with CustomerStatus.NonEmpty
      with scalapb.lenses.Updatable[CustomerRegistered] {
    // ...
  }

  final case class CustomerOneTime(
  ) extends scalapb.GeneratedMessage
      with CustomerStatus.NonEmpty
      with scalapb.lenses.Updatable[CustomerOneTime] {
    // ...
  }

Notice, that while it is flat, it still adds ``CustmerStatus.Empty`` - this is
because this type would be used directly inside the message that contains is
and it would be non-nullable (while the oneof content could still be absent).

Transforming to and from:

.. code-block:: scala

  package order

  sealed trait CustomerStatus
  object CustomerStatus {
    case object CustomerRegistered extends CustomerStatus
    case object CustomerOneTime extends CustomerStatus
  }

could be done with:

.. code-block:: scala

  val domainStatus: order.CustomerStatus = order.CustomerStatus.CustomerRegistered
  val pbStatus: pb.order.CustomerStatus = pb.order.CustomerRegistered()

  domainStatus.into[pb.order.CustomerStatus].transform == pbStatus

  pbStatus
    .intoPartial[order.CustomerStatus]
    .withCoproductInstancePartial[pb.order.CustomerStatus.Empty.type](_ => partial.Result.fromEmpty)
    .withCoproductInstance[pb.order.CustomerStatus.NonEmpty](_.transformInto[order.CustomerStatus])
    .transform
    .asOption == Some(domainStatus)

Notice how we have to manually handle decoding from ``Empty`` value.

sealed_value_optional oneof fields
----------------------------------

If instead of non-nullable type with ``.Empty`` subtype, we would prefer
``Option``\al type without ``.Empty`` subtype, there is optional sealed hierarchy
available. Similarly to non-optional it requires `several conditions <https://scalapb.github.io/docs/sealed-oneofs#optional-sealed-oneof>`_.

When you define it:

.. code-block:: protobuf

  message PaymentStatus {
    oneof sealed_value_optional {
      PaymentRequested requested = 1;
      PaymentCreated created = 2;
      PaymentSucceeded succeeded = 3;
      PaymentFailed failed = 4;
    }
  }

  message PaymentRequested {}

  message PaymentCreated {
    string external_id = 1;
  }

  message PaymentSucceeded {}

  message PaymentFailed {}

and try to map it to and from:

.. code-block:: scala

  package order

  sealed trait PaymentStatus
  object PaymentStatus {
    case object PaymentRequested extends PaymentStatus
    case class PaymentCreated(externalId: String) extends PaymentStatus
    case object PaymentSucceeded extends PaymentStatus
    case object PaymentFailed extends PaymentStatus
  }

the transformation is pretty straightforward both directions:

.. code-block:: scala

  val domainStatus: Option[order.PaymentStatus] = Option(order.PaymentStatus.PaymentRequested)
  val pbStatus: Option[pb.order.PaymentStatus] = Option(pb.order.PaymentRequested())

  domainStatus.into[Option[pb.order.PaymentStatus]].transform ==> pbStatus
  pbStatus.into[Option[order.PaymentStatus]].transform ==> domainStatus

since no ``Empty`` case handling is needed.
