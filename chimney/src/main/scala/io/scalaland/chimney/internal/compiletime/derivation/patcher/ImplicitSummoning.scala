package io.scalaland.chimney.internal.compiletime.derivation.patcher

private[compiletime] trait ImplicitSummoning { this: Derivation =>

  import ChimneyType.Implicits.*

  final protected def summonPatcherSafe[A: Type, Patch: Type](implicit
      ctx: PatcherContext[A, Patch]
  ): Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    if (isForwardReferenceToItself[A, Patch](ctx.config.preventResolutionForTypes)) None
    else summonPatcherUnchecked[A, Patch]

  final protected def summonPatcherUnchecked[A: Type, Patch: Type]
      : Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    Expr.summonImplicit[io.scalaland.chimney.Patcher[A, Patch]]

  // prevents: val t: Patcher[A, B] = (a, b) => t.patch(a, b)
  private def isForwardReferenceToItself[A: Type, Patch: Type](
      preventResolutionForTypes: Option[(??, ??)]
  ): Boolean = preventResolutionForTypes.exists { case (from, to) =>
    from.Underlying =:= Type[A] && to.Underlying =:= Type[Patch]
  }
}
