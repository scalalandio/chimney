package io.scalaland.chimney.internal

import io.scalaland.chimney.PartialTransformer

import scala.collection.compat._
import scala.util.hashing.MurmurHash3

sealed abstract class ErrorsCollection extends Iterable[PartialTransformer.Error] {

  final def prependPath(pathElement: PartialTransformer.PathElement): ErrorsCollection = {
    ErrorsCollection.WrapPath(this, pathElement)
  }

  override final def isEmpty: Boolean = false

  override final def iterator: Iterator[PartialTransformer.Error] = {
    this match {
      case ErrorsCollection.Single(error)      => Iterator.single(error)
      case ErrorsCollection.Wrap(errors)       => errors.iterator
      case ErrorsCollection.Merge(left, right) => left.iterator ++ right.iterator
      case ErrorsCollection.WrapPath(ec, pe)   => ec.iterator.map(_.prependErrorPath(pe))
    }
  }

  final def ++(other: ErrorsCollection): ErrorsCollection = {
    ErrorsCollection.Merge(this, other)
  }

  // TODO: write some tests for making sure that equals/hashCode contract is satisfied
  override final def equals(obj: Any): Boolean = {
    obj match {
      case xs: ErrorsCollection => xs.iterator.sameElements(this.iterator)
      case _                    => false
    }
  }

  override final def hashCode(): Int = {
    MurmurHash3.orderedHash(iterator)
  }
}

object ErrorsCollection {

  final def fromSingle(error: PartialTransformer.Error): ErrorsCollection = Single(error)

  final def from(head: PartialTransformer.Error, tail: PartialTransformer.Error*): ErrorsCollection = {
    if (tail.isEmpty) Single(head)
    else if (tail.sizeIs == 1) Merge(Single(head), Single(tail.head))
    else Merge(Single(head), Wrap(tail))
  }

  final def merge(ec1: ErrorsCollection, ec2: ErrorsCollection): ErrorsCollection = {
    ec1 ++ ec2
  }

  private final case class Single private (error: PartialTransformer.Error) extends ErrorsCollection
  private final case class Wrap private (errors: Iterable[PartialTransformer.Error]) extends ErrorsCollection
  private final case class Merge private (left: ErrorsCollection, right: ErrorsCollection) extends ErrorsCollection
  private final case class WrapPath private (ec: ErrorsCollection, pe: PartialTransformer.PathElement)
      extends ErrorsCollection
}
