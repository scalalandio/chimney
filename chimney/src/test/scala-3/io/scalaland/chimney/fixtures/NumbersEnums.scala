package io.scalaland.chimney.fixtures
package numbers

// following https://en.wikipedia.org/wiki/Names_of_large_numbers

package shortEnums {
  enum NumScale[+A, Dummy]:
    case Zero extends NumScale[Nothing, Nothing]
    case Million[A](count: A) extends NumScale[A, Nothing] // 10^6
    case Billion[A](count: A) extends NumScale[A, Nothing] // 10^9
    case Trillion[A](count: A) extends NumScale[A, Nothing] // 10^12
}

package longEnums {
  enum NumScale[+A]:
    case Zero extends NumScale[Nothing]
    case Million[A](count: A) extends NumScale[A] // 10^6
    case Milliard[A](count: A) extends NumScale[A] // 10^9
    case Billion[A](count: A) extends NumScale[A] // 10^12
    case Billiard[A](count: A) extends NumScale[A] // 10^15
    case Trillion[A](count: A) extends NumScale[A] // 10^18
}

import io.scalaland.chimney.{PartialTransformer, Transformer}

object ScalesEnumsPartialTransformer {

  import io.scalaland.chimney.dsl.*

  implicit def shortToLongTotalInner[A, B](implicit
      ft: Transformer[A, B]
  ): PartialTransformer[shortEnums.NumScale[A, Nothing], longEnums.NumScale[B]] =
    Transformer
      .definePartial[shortEnums.NumScale[A, Nothing], longEnums.NumScale[B]]
      .withSealedSubtypeHandledPartial { (billion: shortEnums.NumScale.Billion[A]) =>
        billion.transformIntoPartial[longEnums.NumScale.Milliard[B]]
      }
      .withSealedSubtypeHandledPartial { (trillion: shortEnums.NumScale.Trillion[A]) =>
        trillion.transformIntoPartial[longEnums.NumScale.Billion[B]]
      }
      .buildTransformer

  implicit def shortToLongPartialInner[A, B](implicit
      ft: PartialTransformer[A, B]
  ): PartialTransformer[shortEnums.NumScale[A, Nothing], longEnums.NumScale[B]] =
    Transformer
      .definePartial[shortEnums.NumScale[A, Nothing], longEnums.NumScale[B]]
      .withSealedSubtypeHandledPartial { (billion: shortEnums.NumScale.Billion[A]) =>
        billion.transformIntoPartial[longEnums.NumScale.Milliard[B]]
      }
      .withSealedSubtypeHandledPartial { (trillion: shortEnums.NumScale.Trillion[A]) =>
        trillion.transformIntoPartial[longEnums.NumScale.Billion[B]]
      }
      .buildTransformer
}
