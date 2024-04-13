package io.scalaland.chimney.internal.compiletime.derivation.transformer

private[compiletime] trait ImplicitSummoning { this: Derivation =>

  import ChimneyType.Implicits.*

  final protected def summonTransformerSafe[From, To](implicit
      ctx: TransformationContext[From, To]
  ): Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    // prevents: val t: Transformer[A, B] = a => t.transform(a)
    if (ctx.config.isImplicitSummoningPreventedFor[From, To]) None
    else summonTransformerUnchecked[From, To]

  final protected def summonPartialTransformerSafe[From, To](implicit
      ctx: TransformationContext[From, To]
  ): Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    // prevents: val t: PartialTransformer[A, B] = a => t.transform(a)
    if (ctx.config.isImplicitSummoningPreventedFor[From, To]) None
    else summonPartialTransformerUnchecked[From, To]

  final protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    Expr.summonImplicit[io.scalaland.chimney.Transformer[From, To]]

  final protected def summonPartialTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    Expr.summonImplicit[io.scalaland.chimney.PartialTransformer[From, To]]

  final protected type OptionalValueOfExpr[Optional, Value] =
    Expr[io.scalaland.chimney.integrations.OptionalValue.Of[Optional, Value]]
  final protected def summonOptionalValue[Optional: Type]: Option[Existential[OptionalValueOfExpr[Optional, *]]] = {
    val lookedUp = ChimneyType.OptionalValueOf.inferred[Optional]
    import lookedUp.Underlying as LookedUp
    Expr.summonImplicit[LookedUp].map { optionalExpr =>
      val ChimneyType.OptionalValueOf(_, value) = optionalExpr.tpe: @unchecked
      import value.Underlying as Value
      Existential[OptionalValueOfExpr[Optional, *], Value](
        optionalExpr.upcastToExprOf[io.scalaland.chimney.integrations.OptionalValue.Of[Optional, Value]]
      )
    }
  }

  final protected type PartiallyBuildIterableOfExpr[Collection, Value] =
    Expr[io.scalaland.chimney.integrations.PartiallyBuildIterable.Of[Collection, Value]]
  final protected def summonPartiallyBuildIterable[Collection: Type]
      : Option[Existential[PartiallyBuildIterableOfExpr[Collection, *]]] = {
    val lookedUp = ChimneyType.PartiallyBuildIterableOf.inferred[Collection]
    import lookedUp.Underlying as LookedUp
    Expr.summonImplicit[LookedUp].map { partiallyBuildIterableExpr =>
      val ChimneyType.PartiallyBuildIterableOf(_, item) = partiallyBuildIterableExpr.tpe: @unchecked
      import item.Underlying as Item
      Existential[PartiallyBuildIterableOfExpr[Collection, *], Item](
        partiallyBuildIterableExpr
          .upcastToExprOf[io.scalaland.chimney.integrations.PartiallyBuildIterable.Of[Collection, Item]]
      )
    }
  }

  final protected type TotallyBuildIterableOfExpr[Collection, Value] =
    Expr[io.scalaland.chimney.integrations.TotallyBuildIterable.Of[Collection, Value]]
  final protected def summonTotallyBuildIterable[Collection: Type]
      : Option[Existential[TotallyBuildIterableOfExpr[Collection, *]]] = {
    val lookedUp = ChimneyType.TotallyBuildIterableOf.inferred[Collection]
    import lookedUp.Underlying as LookedUp
    Expr.summonImplicit[LookedUp].map { totallyBuildIterableExpr =>
      val ChimneyType.TotallyBuildIterableOf(_, item) = totallyBuildIterableExpr.tpe: @unchecked
      import item.Underlying as Item
      Existential[TotallyBuildIterableOfExpr[Collection, *], Item](
        totallyBuildIterableExpr
          .upcastToExprOf[io.scalaland.chimney.integrations.TotallyBuildIterable.Of[Collection, Item]]
      )
    }
  }
}
