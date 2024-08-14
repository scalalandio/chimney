package io.scalaland.chimney.fixtures.lenslike

case class Foo[A](value: A, meta: String)

case class WithOption[A](option: Option[A])

case class WithEither[L, R](either: Either[L, R])

case class WithList[A](list: List[A])

case class WithMap[K, V](map: Map[K, V])
