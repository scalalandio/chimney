package io.scalaland.chimney.validated

import cats.data.{NonEmptyChain, NonEmptyList}

object aliases {
  type V[A] = cats.data.ValidatedNec[String, A]

  type IV[A] = cats.data.ValidatedNec[VTransformer.Error, A]

  type NEC[A] = NonEmptyChain[A]

  val NEC = NonEmptyChain

  type NEL[A] = NonEmptyList[A]

  val NEL = NonEmptyList
}
