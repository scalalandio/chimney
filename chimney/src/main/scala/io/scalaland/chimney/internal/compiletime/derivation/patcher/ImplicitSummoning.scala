package io.scalaland.chimney.internal.compiletime.derivation.patcher

/** Hearth-based port of `...compiletime.derivation.patcher.ImplicitSummoning`.
  *
  * Difference vs the old version: `summonPatcherUnchecked` was abstract here and implemented per platform
  * (`summonIgnoring` twice); Hearth's `Expr.summonImplicitIgnoring` lets it be implemented ONCE, in shared code
  * (mirrors the transformer's [[transformer.ImplicitSummoning]] port).
  */
private[compiletime] trait ImplicitSummoning { this: Derivation & hearth.MacroCommons =>

  import ChimneyType.Implicits.*

  final protected def summonPatcherSafe[A: Type, Patch: Type](implicit
      ctx: TransformationContext[Patch, A]
  ): Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    if (ctx.config.isImplicitSummoningPreventedFor[Patch, A]) None
    else summonPatcherUnchecked[A, Patch]

  private lazy val ignoredPatcherImplicits: List[UntypedMethod] = {
    implicit val PatcherModule: Type[io.scalaland.chimney.Patcher.type] = Type.of[io.scalaland.chimney.Patcher.type]
    val wanted = Set(
      "derive" // handled by recursion in macro
    )
    Type[io.scalaland.chimney.Patcher.type].methods.collect { case method if wanted(method.name) => method.asUntyped }
  }

  protected def summonPatcherUnchecked[A: Type, Patch: Type]: Option[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    Expr.summonImplicitIgnoring[io.scalaland.chimney.Patcher[A, Patch]](ignoredPatcherImplicits*).toOption
}
