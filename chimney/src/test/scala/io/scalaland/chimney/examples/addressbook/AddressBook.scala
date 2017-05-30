package io.scalaland.chimney.examples.addressbook

case class PersonName(name: String) extends AnyVal
case class PersonId(id: Int) extends AnyVal
case class Email(email: String) extends AnyVal

sealed trait PhoneType
case object `PhoneType.MOBILE` extends PhoneType
case object `PhoneType.HOME` extends PhoneType
case object `PhoneType.WORK` extends PhoneType


object Person {

  case class PhoneNumber(number: String,
                         `type`: PhoneType)
}

case class Person(name: PersonName,
                  id: PersonId,
                  email: Email,
                  phones: List[Person.PhoneNumber])




case class AddressBook(people: List[Person])
