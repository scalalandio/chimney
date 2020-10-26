package io.scalaland.chimney.internal.utils

trait EitherUtils {

  implicit class MapOps[K, E, V](map: Map[K, Either[E, V]]) {

    def partitionEitherValues: (Map[K, E], Map[K, V]) = {
      val (lefts, rights) = map.partition(_._2.isLeft)
      (
        lefts.collect { case (k, Left(v))   => k -> v },
        rights.collect { case (k, Right(v)) => k -> v }
      )
    }
  }
}

object EitherUtils extends EitherUtils
