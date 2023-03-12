package io.scalaland.chimney.examples

package colors1 {
  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
}

package colors2 {
  sealed trait Color
  case object Blue extends Color
  case object Green extends Color
  case object Red extends Color
  case object Black extends Color
}

package colors3 {
  sealed trait Color
  sealed trait SimpleColor extends Color
  sealed trait ComplexedColor extends Color
  case object Red extends SimpleColor
  case object Green extends SimpleColor
  case object Blue extends SimpleColor
  case object Black extends ComplexedColor
}
