package io.scalaland.chimney.example

trait MyTypeClass[From, To] {

  def convert(from: From): To
}
object MyTypeClass extends MyTypeClassCompanionPlatform
