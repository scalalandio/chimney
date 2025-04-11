package io.scalaland.chimney.internal.compiletime.derivation.patcher

private[compiletime] trait Contexts { this: Derivation =>

  /** Stores all the "global" information that might be needed: types used, user configuration, runtime values, etc */
  final protected case class PatcherContext[A, Patch](obj: Expr[A], patch: Expr[Patch])(
      val A: Type[A],
      val Patch: Type[Patch],
      val config: PatcherConfiguration,
      val derivationStartedAt: java.time.Instant
  ) {

    final type Target = A
    val Target = A

    def updateConfig(update: PatcherConfiguration => PatcherConfiguration): this.type =
      PatcherContext(obj, patch)(
        A = A,
        Patch = Patch,
        config = update(config),
        derivationStartedAt = derivationStartedAt
      )
        .asInstanceOf[this.type]

    def toTransformerContext: TransformationContext.ForTotal[Patch, A] = {
      implicit val self: PatcherContext[A, Patch] = this // for A: Type, Patch: Type
      TransformationContext.ForTotal[Patch, A](patch)(
        From = Patch,
        To = A,
        srcJournal = Vector(Path.Root -> patch.as_??),
        tgtJournal = Vector(Path.Root),
        config = config.toTransformerConfiguration(obj.as_??),
        derivationStartedAt = derivationStartedAt
      )
    }

    override def toString: String =
      s"PatcherContext[A = ${Type.prettyPrint(using A)}, Patch = ${Type
          .prettyPrint(using Patch)}](obj = ${Expr.prettyPrint(obj)}, patch = ${Expr.prettyPrint(patch)})($config)"
  }
  protected object PatcherContext {

    def create[A: Type, Patch: Type](
        obj: Expr[A],
        patch: Expr[Patch],
        config: PatcherConfiguration
    ): PatcherContext[A, Patch] =
      PatcherContext(obj = obj, patch = patch)(
        A = Type[A],
        Patch = Type[Patch],
        config = config.preventImplicitSummoningFor[A, Patch],
        derivationStartedAt = java.time.Instant.now()
      )
  }

  protected object Patched {

    def unapply[Patch, A](implicit ctx: TransformationContext[Patch, A]): Option[Expr[A]] =
      ctx.config.filterCurrentOverridesForFallbacks.collectFirst {
        case TransformerOverride.Fallback(fallback) if fallback.Underlying =:= Type[A] =>
          fallback.value.asInstanceOf[Expr[A]]
      }
  }

  // unpacks Types from Contexts
  implicit final protected def ctx2AType[A, Patch](implicit ctx: PatcherContext[A, Patch]): Type[A] = ctx.A
  implicit final protected def ctx2PatchType[A, Patch](implicit ctx: PatcherContext[A, Patch]): Type[Patch] = ctx.Patch

  // for unpacking Exprs from Context, import ctx.* should be enough
}
