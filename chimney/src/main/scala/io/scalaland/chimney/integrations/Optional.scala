package io.scalaland.chimney.integrations

trait Optional[OptionalOf[_]] {

  def map[A, B](oa: OptionalOf[A], f: A => B): OptionalOf[B]

  def fold[A, B](oa: OptionalOf[A], onNone: => B, onSome: A => B): B

  def getOrElse[A](oa: OptionalOf[A], onNone: => A): A = fold(oa, onNone, identity)

  def orElse[A](oa: OptionalOf[A], onNone: => OptionalOf[A]): OptionalOf[A] = fold(oa, onNone, _ => oa)
}
