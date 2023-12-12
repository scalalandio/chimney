package io.scalaland.chimney.integrations

// TODO: add deprecated type alias in io.scalaland.chimney.javacollections
trait IteratorOf[A, CC] {
  def iterator(collection: CC): Iterator[A]

  final def foreach(collection: CC)(f: A => Any): Unit = iterator(collection).foreach(f)
}
