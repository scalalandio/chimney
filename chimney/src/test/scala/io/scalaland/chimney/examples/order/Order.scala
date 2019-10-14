package io.scalaland.chimney.examples.order

case class Item(id: Int, name: String)
case class OrderLine(item: Item, quantity: Int)
case class Address(street: String, zipCode: Int, city: String)
case class Customer(id: Int, firstName: String, lastName: String, address: Address)
case class Order(lines: List[OrderLine], customer: Customer)
