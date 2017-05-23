package io.scalaland.chimney

import shapeless.Witness

sealed trait Modifier

object Modifier {
  sealed trait empty extends Modifier
  case object empty extends empty

  case class fieldValue[L <: Symbol, T](label: Witness.Aux[L],
                                        value: T)
    extends Modifier

  case class fieldFunction[L <: Symbol, T, F](label: Witness.Aux[L],
                                              f: T => F)
    extends Modifier

  case class relabel[L1 <: Symbol, L2 <: Symbol](label1: Witness.Aux[L1],
                                                 label2: Witness.Aux[L2])
    extends Modifier
}
