package io.scalaland.chimney.examples

package trip {

  case class PersonForm(name: String, age: String, height: String)
  case class Person(name: String, age: Int, height: Double)

  case class TripForm(tripId: String, people: List[PersonForm])
  case class Trip(id: Int, people: Vector[Person])

  case class User(name: String, age: Int, height: Double)
}
