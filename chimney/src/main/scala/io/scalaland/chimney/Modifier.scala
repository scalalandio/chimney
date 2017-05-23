package io.scalaland.chimney


sealed trait Modifier

object Modifier {

  class fieldFunction[L <: Symbol, From, T](val f: From => T)
    extends Modifier

  class relabel[L1 <: Symbol, L2 <: Symbol]
    extends Modifier
}
