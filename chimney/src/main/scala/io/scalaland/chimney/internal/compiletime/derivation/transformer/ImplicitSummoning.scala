package io.scalaland.chimney.internal.compiletime.derivation.transformer

trait ImplicitSummoning { this: Derivation =>

  import ChimneyTypeImplicits.*

  final protected def summonTransformerSafe[From, To](implicit
      ctx: TransformerContext[From, To]
  ): Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    if (ctx.config.preventResolutionForTypes.contains((Type[From].asComputed, Type[From].asComputed))) None
    else summonTransformerUnchecked[From, To].filterNot(isAutoderivedFromTransformerDerive(_))

  final protected def summonPartialTransformerSafe[From, To](implicit
      ctx: TransformerContext[From, To]
  ): Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    if (ctx.config.preventResolutionForTypes.contains((Type[From].asComputed, Type[From].asComputed))) None
    else summonPartialTransformerUnchecked[From, To].filterNot(isAutoderivedFromPartialTransformerDerive(_))

  final protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    Expr.summonImplicit[io.scalaland.chimney.Transformer[From, To]]

  final protected def summonPartialTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    Expr.summonImplicit[io.scalaland.chimney.PartialTransformer[From, To]]

  protected def isAutoderivedFromTransformerDerive[From: Type, To: Type](
      expr: Expr[io.scalaland.chimney.Transformer[From, To]]
  ): Boolean

  protected def isAutoderivedFromPartialTransformerDerive[From: Type, To: Type](
      expr: Expr[io.scalaland.chimney.PartialTransformer[From, To]]
  ): Boolean
}
