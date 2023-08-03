package io.scalaland.chimney.fixtures

object colors1enums {
  enum Color:
    case Red, Blue, Green
}

object colors2enums {
  enum Color:
    case Blue, Green, Red, Black
}

package colors3enums {
  sealed trait Color
  enum SimpleColor extends Color:
    case Red, Green, Blue
  enum ComplexColor extends Color:
    case Black
}
