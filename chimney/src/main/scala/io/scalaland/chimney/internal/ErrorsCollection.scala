package io.scalaland.chimney.internal

import io.scalaland.chimney.PartialTransformer
import scala.collection.compat._

sealed abstract class ErrorsCollection extends Iterable[PartialTransformer.Error] {

  override final def isEmpty: Boolean = {
    this.isInstanceOf[ErrorsCollection.Empty.type]
  }

  override final def iterator: Iterator[PartialTransformer.Error] = {
    this match {
      case _: ErrorsCollection.Empty.type      => Iterator.empty
      case ErrorsCollection.Single(error)      => Iterator.single(error)
      case ErrorsCollection.Wrap(errors)       => errors.iterator
      case ErrorsCollection.Merge(left, right) => left.iterator ++ right.iterator
    }
  }

  final def ++(other: ErrorsCollection): ErrorsCollection = {
    if (other.isEmpty) this
    else if (this.isEmpty) other
    else ErrorsCollection.Merge(this, other)
  }

  override final def equals(obj: Any): Boolean = {
    obj match {
      case xs: Iterable[_] if xs.isEmpty                     => this.isEmpty
      case xs: Iterable[PartialTransformer.Error] @unchecked => xs.iterator.sameElements(this.iterator)
      case _                                                 => false
    }
  }
}

object ErrorsCollection {
  final val empty: ErrorsCollection = Empty

  final def fromSingle(error: PartialTransformer.Error): ErrorsCollection = Single(error)

  final def fromIterable(iterable: Iterable[PartialTransformer.Error]): ErrorsCollection = {
    iterable match {
      case ec: ErrorsCollection      => ec
      case _ if iterable.isEmpty     => ErrorsCollection.empty
      case _ if iterable.sizeIs == 1 => Single(iterable.head)
      case _                         => Wrap(iterable)
    }
  }

  private case object Empty extends ErrorsCollection
  private final case class Single(error: PartialTransformer.Error) extends ErrorsCollection
  private final case class Wrap(errors: Iterable[PartialTransformer.Error]) extends ErrorsCollection
  private final case class Merge(left: ErrorsCollection, right: ErrorsCollection) extends ErrorsCollection
}
