Unsafe options transformation
=============================

Chimney supports opt-in unsafe transformation from ``Option[T]`` to
``T`` if enabled explicitly with ``.enableUnsafeOption``.

.. warning::

  Transforming ``None`` into a concrete value will lead to
  ``NoSuchElementException`` at runtime, so use at your own risk.

Proto3 motivational example
---------------------------

Unsafe option mode is typically useful when mapping proto3-generated
classes to domain classes. `ScalaPB <https://scalapb.github.io>`_ indeed
generates fields wrapped in ``Option`` types for messages. In certain
scenarios, it can be safe to assume that the value is always present,
thus allowing for significant boilerplate reduction.

Here's an example protobuf definition.

.. code-block:: proto

  syntax = "proto3";
  package pb;
  message Item {
      int32 id = 1;
      string name = 2;
  }
  message OrderLine {
      Item item = 1;
      int32 quantity = 2;
  }
  message Address {
      string street = 1;
      int32 zip_code = 2;
      string city = 3;
  }
  message Customer {
      int32 id = 1;
      string first_name = 2;
      string last_name = 3;
      Address address = 4;
  }
  message Order {
      repeated OrderLine lines = 1;
      Customer customer = 2;
  }

And the equivalent domain model definitions:

.. code-block:: scala

  package domain

  case class Item(id: Int, name: String)
  case class OrderLine(item: Item, quantity: Int)
  case class Address(street: String, zipCode: Int, city: String)
  case class Customer(id: Int, firstName: String, lastName: String, address: Address)
  case class Order(lines: List[OrderLine], customer: Customer)


Enabling unsafe option extraction
---------------------------------

Transforming from one representation to the other can be achieved directly
using ``.enableUnsafeOption``.

.. code-block:: scala

  val domainOrder = pbOrder.into[domain.Order]
    .enableUnsafeOption
    .transform

and vice-versa:

.. code-block:: scala

  val pbOrder = domainOrder.into[pb.Order]
    .enableUnsafeOption
    .transform

