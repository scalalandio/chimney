package io.scalaland.chimney.fixtures.valuetypes

case class UserName(value: String) extends AnyVal
case class UserNameAlias(value: String) extends AnyVal
case class UserId(value: Int) extends AnyVal

case class UserDTO(id: String, name: String)
case class User(id: String, name: UserName)
case class UserAlias(id: String, name: UserNameAlias)

case class UserWithUserName(id: UserName)
case class UserWithName(id: String)
case class UserWithUserId(id: UserId)
case class UserWithId(id: Int)
