package io.scalaland.chimney.examples.order

case class Item(id: Int, name: String)
case class OrderLine(item: Item, quantity: Int)
case class Address(street: String, zipCode: Int, city: String)
case class Customer(id: Int, firstName: String, lastName: String, address: Address)
case class Order(lines: List[OrderLine], customer: Customer)

sealed trait CustomerStatus
object CustomerStatus {
  case object CustomerRegistered extends CustomerStatus
  case object CustomerOneTime extends CustomerStatus
}

sealed trait PaymentStatus
object PaymentStatus {
  case object PaymentRequested extends PaymentStatus
  case class PaymentCreated(externalId: String) extends PaymentStatus
  case object PaymentSucceeded extends PaymentStatus
  case object PaymentFailed extends PaymentStatus
}
