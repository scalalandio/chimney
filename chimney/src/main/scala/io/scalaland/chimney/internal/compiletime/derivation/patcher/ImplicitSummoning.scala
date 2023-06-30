package io.scalaland.chimney.internal.compiletime.derivation.patcher

private[compiletime] trait ImplicitSummoning { this: Derivation =>

  import ChimneyType.Implicits.*

  final protected def summonPatcherUnchecked[A: Type, Patch: Type]
  : Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    Expr.summonImplicit[io.scalaland.chimney.Patcher[A, Patch]]
}
