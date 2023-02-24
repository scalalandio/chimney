package io.scalaland.chimney.examples.valuetypes

case class UserName(value: String) extends AnyVal
case class UserDTO(id: String, name: String)
case class User(id: String, name: UserName)
