package io.scalaland.chimney.internal.compiletime.derivation.transformer

trait ImplicitSummoning { this: Derivation =>

  final protected def summonTransformer[From, To](implicit
      ctx: TransformerContext[From, To]
  ): Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    if (ctx.config.preventResolutionForTypes.contains((Type[From].asComputed, Type[From].asComputed))) None
    else summonTransformerUnchecked[From, To]

  final protected def summonPartialTransformer[From, To](implicit
      ctx: TransformerContext[From, To]
  ): Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    if (ctx.config.preventResolutionForTypes.contains((Type[From].asComputed, Type[From].asComputed))) None
    else summonPartialTransformerUnchecked[From, To]

  protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]]

  protected def summonPartialTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]]
}
