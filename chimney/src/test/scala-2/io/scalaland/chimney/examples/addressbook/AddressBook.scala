package io.scalaland.chimney.examples.addressbook

case class PersonName(name: String) extends AnyVal
case class PersonId(id: Int) extends AnyVal
case class Email(email: String) extends AnyVal

sealed trait PhoneType

case object MOBILE extends PhoneType
case object HOME extends PhoneType
case object WORK extends PhoneType

case class PhoneNumber(number: String, `type`: PhoneType)

case class Person(name: PersonName, id: PersonId, email: Email, phones: List[PhoneNumber])

case class AddressBook(people: List[Person])
