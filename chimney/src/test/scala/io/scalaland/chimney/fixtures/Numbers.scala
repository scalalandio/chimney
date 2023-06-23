package io.scalaland.chimney.fixtures

package numbers {

  // following https://en.wikipedia.org/wiki/Names_of_large_numbers

  package short {
    sealed trait NumScale[+T, Dummy]
    case object Zero extends NumScale[Nothing, Nothing]
    case class Million[T](count: T) extends NumScale[T, Nothing] // 10^6
    case class Billion[T](count: T) extends NumScale[T, Nothing] // 10^9
    case class Trillion[T](count: T) extends NumScale[T, Nothing] // 10^12
  }

  package long {
    sealed trait NumScale[+T]
    case object Zero extends NumScale[Nothing]
    case class Million[T](count: T) extends NumScale[T] // 10^6
    case class Milliard[T](count: T) extends NumScale[T] // 10^9
    case class Billion[T](count: T) extends NumScale[T] // 10^12
    case class Billiard[T](count: T) extends NumScale[T] // 10^15
    case class Trillion[T](count: T) extends NumScale[T] // 10^18
  }

  import io.scalaland.chimney.{PartialTransformer, Transformer}

  object ScalesPartialTransformer {

    import io.scalaland.chimney.dsl.*
    import io.scalaland.chimney.partial

    implicit def shortToLongTotalInner[A, B](implicit
        ft: Transformer[A, B]
    ): PartialTransformer[short.NumScale[A, Nothing], long.NumScale[B]] = (a: short.NumScale[A, Nothing], _) =>
      a match {
        case short.Zero                  => partial.Result.fromValue(long.Zero)
        case million: short.Million[A]   => million.transformIntoPartial[long.Million[B]]
        case billion: short.Billion[A]   => billion.transformIntoPartial[long.Milliard[B]]
        case trillion: short.Trillion[A] => trillion.transformIntoPartial[long.Billion[B]]
      }

    implicit def shortToLongPartialInner[A, B](implicit
        ft: PartialTransformer[A, B]
    ): PartialTransformer[short.NumScale[A, Nothing], long.NumScale[B]] = (a: short.NumScale[A, Nothing], _) =>
      a match {
        case short.Zero                  => partial.Result.fromValue(long.Zero)
        case million: short.Million[A]   => million.transformIntoPartial[long.Million[B]]
        case billion: short.Billion[A]   => billion.transformIntoPartial[long.Milliard[B]]
        case trillion: short.Trillion[A] => trillion.transformIntoPartial[long.Billion[B]]
      }

    // FIXME
    /*
    implicit def shortToLongTotalInner[A, B](implicit
        ft: Transformer[A, B]
    ): PartialTransformer[short.NumScale[A, Nothing], long.NumScale[B]] = {
      Transformer
        .definePartial[short.NumScale[A, Nothing], long.NumScale[B]]
        .withCoproductInstancePartial { (billion: short.Billion[A]) =>
          billion.transformIntoPartial[long.Milliard[B]]
        }
        .withCoproductInstancePartial { (trillion: short.Trillion[A]) =>
          trillion.transformIntoPartial[long.Billion[B]]
        }
        .buildTransformer
    }

    implicit def shortToLongPartialInner[A, B](implicit
        ft: PartialTransformer[A, B]
    ): PartialTransformer[short.NumScale[A, Nothing], long.NumScale[B]] = {
      Transformer
        .definePartial[short.NumScale[A, Nothing], long.NumScale[B]]
        .withCoproductInstancePartial { (billion: short.Billion[A]) =>
          billion.transformIntoPartial[long.Milliard[B]]
        }
        .withCoproductInstancePartial { (trillion: short.Trillion[A]) =>
          trillion.transformIntoPartial[long.Billion[B]]
        }
        .buildTransformer
    }
     */
  }
}
