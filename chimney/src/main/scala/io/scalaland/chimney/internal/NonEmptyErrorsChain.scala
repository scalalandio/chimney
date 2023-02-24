package io.scalaland.chimney.internal

import io.scalaland.chimney.partial

import scala.collection.compat._
import scala.util.hashing.MurmurHash3

/** Specialized chain-like data structure for efficient representation of path-annotated errors,
  * guaranteed to be non-empty.
  *
  * @since 0.7.0
  */
sealed abstract class NonEmptyErrorsChain extends Iterable[partial.Error] {

  /** Prepends path of all the errors in collection with a given path element.
    *
    * @param pathElement path element to be prepended
    * @return instance of [[NonEmptyErrorsChain]]
    *
    * @since 0.7.0
    */
  final def prependPath(pathElement: partial.PathElement): NonEmptyErrorsChain = {
    NonEmptyErrorsChain.WrapPath(this, pathElement)
  }

  /** Tests whether collections is empty.
    *
    * @return always false, as the collection is guaraneed to be non-empty
    *
    * @since 0.7.0
    */
  override final def isEmpty: Boolean = false

  /** Errors iterator.
    *
    * @since 0.7.0
    */
  override final def iterator: Iterator[partial.Error] = {
    this match {
      case NonEmptyErrorsChain.Single(error)                 => Iterator.single(error)
      case NonEmptyErrorsChain.Wrap(errors)                  => errors.iterator
      case NonEmptyErrorsChain.Merge(left, right)            => left.iterator ++ right.iterator
      case NonEmptyErrorsChain.WrapPath(errors, pathElement) => errors.iterator.map(_.prependErrorPath(pathElement))
    }
  }

  /** Returns a new errors collection containing elements from this, followed by elements of other collection.
    *
    * @param other errors collection
    * @return instance of [[NonEmptyErrorsChain]]
    *
    * @since 0.7.0
    */
  final def ++(other: NonEmptyErrorsChain): NonEmptyErrorsChain = {
    NonEmptyErrorsChain.Merge(this, other)
  }

  override final def equals(obj: Any): Boolean = {
    obj match {
      case xs: NonEmptyErrorsChain => xs.iterator.sameElements(this.iterator)
      case _                       => false
    }
  }

  override final def hashCode(): Int = {
    MurmurHash3.orderedHash(iterator)
  }
}

object NonEmptyErrorsChain {

  /** Creates a singleton errors collection from an error.
    *
    * @param error error
    * @return instance of [[NonEmptyErrorsChain]]
    * @since 0.7.0
    */
  final def single(error: partial.Error): NonEmptyErrorsChain = {
    Single(error)
  }

  /** Creates errors collection from head and tail.
    *
    * @param head error
    * @param tail errors
    * @return instance of [[NonEmptyErrorsChain]]
    * @since 0.7.0
    */
  final def from(head: partial.Error, tail: partial.Error*): NonEmptyErrorsChain = {
    if (tail.isEmpty) Single(head)
    else if (tail.sizeIs == 1) Merge(Single(head), Single(tail.head))
    else Merge(Single(head), Wrap(tail))
  }

  private final case class Single private (error: partial.Error) extends NonEmptyErrorsChain
  private final case class Wrap private (errors: Iterable[partial.Error]) extends NonEmptyErrorsChain
  private final case class Merge private (errors1: NonEmptyErrorsChain, errors2: NonEmptyErrorsChain)
      extends NonEmptyErrorsChain
  private final case class WrapPath private (errors: NonEmptyErrorsChain, pathElement: partial.PathElement)
      extends NonEmptyErrorsChain
}
