package io.scalaland.chimney.internal.compiletime.derivation.patcher

private[compiletime] trait ImplicitSummoning { this: Derivation =>

  import ChimneyType.Implicits.*

  final protected def summonPatcherSafe[A: Type, Patch: Type](implicit
      ctx: TransformationContext[Patch, A]
  ): Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    if (ctx.config.isImplicitSummoningPreventedFor[Patch, A]) None
    else summonPatcherUnchecked[A, Patch]

  // Not final to override it on Scala 3 with summonIgnoring!

  protected def summonPatcherUnchecked[A: Type, Patch: Type]: Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    Expr.summonImplicit[io.scalaland.chimney.Patcher[A, Patch]]
}
