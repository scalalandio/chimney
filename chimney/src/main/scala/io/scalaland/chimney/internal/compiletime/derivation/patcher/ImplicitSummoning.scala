package io.scalaland.chimney.internal.compiletime.derivation.patcher

private[compiletime] trait ImplicitSummoning { this: Derivation =>

  import ChimneyType.Implicits.*

  final protected def summonPatcherSafe[A: Type, Patch: Type](implicit
      ctx: PatcherContext[A, Patch]
  ): Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    if (ctx.config.isImplicitSummoningPreventedFor[A, Patch]) None
    else summonPatcherUnchecked[A, Patch]

  final protected def summonPatcherUnchecked[A: Type, Patch: Type]
      : Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    Expr.summonImplicit[io.scalaland.chimney.Patcher[A, Patch]]
}
