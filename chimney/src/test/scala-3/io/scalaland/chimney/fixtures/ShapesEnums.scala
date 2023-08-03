package io.scalaland.chimney.fixtures

package shapes1enums {

  case class Point(x: Int, y: Int)

  enum Shape:
    case Triangle(p1: Point, p2: Point, p3: Point)
    case Rectangle(p1: Point, p2: Point)
}

package shapes2enums {

  case class Point(x: Int, y: Int)

  enum Shape:
    case Polygon(points: List[Point])
}

package shapes3enums {

  case class Point(x: Double, y: Double)

  enum Shape:
    case Triangle(p3: Point, p2: Point, p1: Point)
    case Rectangle(p1: Point, p2: Point)
}

package shapes4enums {

  case class Point(x: Double, y: Double)

  sealed trait Shape

  enum ThreeAnglesShape extends Shape:
    case Triangle(p3: Point, p2: Point, p1: Point)

  enum FourAnglesShape extends Shape:
    case Rectangle(p1: Point, p2: Point)
}

package shapes5enums {
  case class Point(x: Double, y: Double)

  sealed trait Shape
  enum Outer extends Shape:
    case Triangle(p1: Point, p2: Point, p3: Point)
    case Rectangle(p1: Point, p2: Point)
    case Circle(center: Point, rad: Double)
  export Outer.{Circle, Rectangle, Triangle}
  enum Inner extends Shape:
    case Triangle(p1: Point, p2: Point, p3: Point)
    case Rectangle(p1: Point, p2: Point)
    case Circle(center: Point, rad: Double)
}

package shapes6enums {
  case class Point(x: Int, y: Int)

  enum Shape:
    case Triangle(value: shapes6enums.Triangle)
    case Rectangle(value: shapes6enums.Rectangle)

  case class Triangle(p1: Point, p2: Point, p3: Point)
  case class Rectangle(p1: Point, p2: Point)
}
