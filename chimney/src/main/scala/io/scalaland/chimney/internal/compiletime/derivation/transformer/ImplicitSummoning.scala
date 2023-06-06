package io.scalaland.chimney.internal.compiletime.derivation.transformer

private[compiletime] trait ImplicitSummoning { this: Derivation =>

  import ChimneyTypeImplicits.*

  final protected def summonTransformerSafe[From, To](implicit
      ctx: TransformationContext[From, To]
  ): Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    if (isForwardReferenceToItself[From, To](ctx.config.preventResolutionForTypes)) None
    else summonTransformerUnchecked[From, To].filterNot(isAutoderivedFromTransformerDerive(_))

  final protected def summonPartialTransformerSafe[From, To](implicit
      ctx: TransformationContext[From, To]
  ): Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    if (isForwardReferenceToItself[From, To](ctx.config.preventResolutionForTypes)) None
    else summonPartialTransformerUnchecked[From, To].filterNot(isAutoderivedFromPartialTransformerDerive(_))

  final protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    Expr.summonImplicit[io.scalaland.chimney.Transformer[From, To]]

  final protected def summonPartialTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    Expr.summonImplicit[io.scalaland.chimney.PartialTransformer[From, To]]

  // prevents: transformer.transform(a):B when we can inline result, and with passing configs down
  protected def isAutoderivedFromTransformerDerive[From: Type, To: Type](
      expr: Expr[io.scalaland.chimney.Transformer[From, To]]
  ): Boolean

  // prevents: pTransformer.transform(a):partial.Result[B] when we can inline result, and with passing configs down
  protected def isAutoderivedFromPartialTransformerDerive[From: Type, To: Type](
      expr: Expr[io.scalaland.chimney.PartialTransformer[From, To]]
  ): Boolean

  // prevents: val t: Transformer[A, B] = a => t.transform(a)
  private def isForwardReferenceToItself[From: Type, To: Type](
      preventResolutionForTypes: Option[(ExistentialType, ExistentialType)]
  ): Boolean = preventResolutionForTypes.exists { case (from, to) =>
    from.Underlying =:= Type[From] && to.Underlying =:= Type[To]
  }
}
