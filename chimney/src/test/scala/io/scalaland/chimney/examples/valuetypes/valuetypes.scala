package io.scalaland.chimney.examples.valuetypes

case class UserName(value: String) extends AnyVal
case class UserNameAlias(value: String) extends AnyVal
case class UserId(value: Int) extends AnyVal

case class UserDTO(id: String, name: String)
case class User(id: String, name: UserName)
case class UserAlias(id: String, name: UserNameAlias)

case class UserWithName(id: UserName)
case class UserWithId(id: UserId)
