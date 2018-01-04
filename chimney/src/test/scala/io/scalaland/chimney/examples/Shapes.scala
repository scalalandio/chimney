package io.scalaland.chimney.examples

package shapes1 {

  case class Point(x: Int, y: Int)

  sealed trait Shape
  case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape
  case class Rectangle(p1: Point, p2: Point) extends Shape
}

package shapes2 {

  case class Point(x: Int, y: Int)

  sealed trait Shape
  case class Polygon(points: List[Point]) extends Shape
}

package shapes3 {

  case class Point(x: Double, y: Double)

  sealed trait Shape
  case class Triangle(p3: Point, p2: Point, p1: Point) extends Shape
  case class Rectangle(p1: Point, p2: Point) extends Shape
}
