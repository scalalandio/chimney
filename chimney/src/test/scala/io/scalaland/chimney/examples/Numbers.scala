package io.scalaland.chimney.examples

package numbers {

  import io.scalaland.chimney.{Transformer, TransformerF, TransformerFSupport}

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

  object ScalesTransformer {

    import io.scalaland.chimney.dsl._

    implicit def shortToLongPureInner[F[+_]: TransformerFSupport, A, B](
        implicit ft: Transformer[A, B]
    ): TransformerF[F, short.NumScale[A, Nothing], long.NumScale[B]] = {
      Transformer
        .defineF[F, short.NumScale[A, Nothing], long.NumScale[B]]
        .withCoproductInstanceF { billion: short.Billion[A] =>
          billion.transformIntoF[F, long.Milliard[B]]
        }
        .withCoproductInstanceF { trillion: short.Trillion[A] =>
          trillion.transformIntoF[F, long.Billion[B]]
        }
        .buildTransformer
    }

    implicit def shortToLongWrappedInner[F[+_]: TransformerFSupport, A, B](
        implicit ft: TransformerF[F, A, B]
    ): TransformerF[F, short.NumScale[A, Nothing], long.NumScale[B]] = {
      Transformer
        .defineF[F, short.NumScale[A, Nothing], long.NumScale[B]]
        .withCoproductInstanceF { billion: short.Billion[A] =>
          billion.transformIntoF[F, long.Milliard[B]]
        }
        .withCoproductInstanceF { trillion: short.Trillion[A] =>
          trillion.transformIntoF[F, long.Billion[B]]
        }
        .buildTransformer
    }
  }
}
