package io.scalaland.chimney.fixtures.user

import io.scalaland.chimney.fixtures.addressbook.{Email, PersonId, PersonName, PhoneNumber}

case class User(name: PersonName, id: PersonId, email: Email, phones: List[PhoneNumber])
