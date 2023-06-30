package io.scalaland.chimney.internal.compiletime.derivation.patcher

private[compiletime] trait Contexts { this: Derivation =>

  final case class PatcherContext[A, Patch](obj: Expr[A], patch: Expr[Patch])(
      val A: Type[A],
      val Patch: Type[Patch]
  ) {

    final type Target = A
    val Target = A
  }

  object PatcherContext {

    def create[A: Type, Patch: Type](obj: Expr[A], patch: Expr[Patch]): PatcherContext[A, Patch] =
      PatcherContext(obj = obj, patch = patch)(
        A = Type[A],
        Patch = Type[Patch]
      )
  }

  // unpacks Types from Contexts
  implicit final protected def ctx2AType[A, Patch](implicit ctx: PatcherContext[A, Patch]): Type[A] = ctx.A
  implicit final protected def ctx2PatchType[A, Patch](implicit ctx: PatcherContext[A, Patch]): Type[Patch] = ctx.Patch

  // for unpacking Exprs from Context, import ctx.* should be enough
}
