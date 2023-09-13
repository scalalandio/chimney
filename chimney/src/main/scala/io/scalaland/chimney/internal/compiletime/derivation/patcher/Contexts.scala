package io.scalaland.chimney.internal.compiletime.derivation.patcher

private[compiletime] trait Contexts { this: Derivation =>

  /** Stores all the "global" information that might be needed: types used, user configuration, runtime values, etc */
  final case class PatcherContext[A, Patch](obj: Expr[A], patch: Expr[Patch])(
      val A: Type[A],
      val Patch: Type[Patch],
      val config: PatcherConfig,
      val derivationStartedAt: java.time.Instant
  ) {

    final type Target = A
    val Target = A

    def updateConfig(update: PatcherConfig => PatcherConfig): this.type =
      PatcherContext(obj, patch)(
        A = A,
        Patch = Patch,
        config = update(config),
        derivationStartedAt = derivationStartedAt
      )
        .asInstanceOf[this.type]
  }
  object PatcherContext {

    def create[A: Type, Patch: Type](
        obj: Expr[A],
        patch: Expr[Patch],
        config: PatcherConfig
    ): PatcherContext[A, Patch] =
      PatcherContext(obj = obj, patch = patch)(
        A = Type[A],
        Patch = Type[Patch],
        config = config.withDefinitionScope(Type[A].as_?? -> Type[Patch].as_??),
        derivationStartedAt = java.time.Instant.now()
      )
  }

  // unpacks Types from Contexts
  implicit final protected def ctx2AType[A, Patch](implicit ctx: PatcherContext[A, Patch]): Type[A] = ctx.A
  implicit final protected def ctx2PatchType[A, Patch](implicit ctx: PatcherContext[A, Patch]): Type[Patch] = ctx.Patch

  // for unpacking Exprs from Context, import ctx.* should be enough
}
