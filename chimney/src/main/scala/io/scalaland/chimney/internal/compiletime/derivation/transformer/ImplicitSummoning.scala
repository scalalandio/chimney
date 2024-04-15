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

  final protected type OptionalValueExpr[Optional, Value] =
    Expr[io.scalaland.chimney.integrations.OptionalValue[Optional, Value]]
  final protected def summonOptionalValue[Optional: Type]: Option[Existential[OptionalValueExpr[Optional, *]]] = {
    val inferred = ChimneyType.OptionalValue.inferred[Optional]
    import inferred.Underlying as Inferred
    Expr.summonImplicit[Inferred].map { optionalExpr =>
      val ChimneyType.OptionalValue(_, value) = optionalExpr.tpe: @unchecked
      import value.Underlying as Value
      Existential[OptionalValueExpr[Optional, *], Value](
        optionalExpr.asInstanceOf[Expr[io.scalaland.chimney.integrations.OptionalValue[Optional, Value]]]
      )
    }
  }

  final protected type PartiallyBuildIterableExpr[Collection, Value] =
    Expr[io.scalaland.chimney.integrations.PartiallyBuildIterable[Collection, Value]]
  final protected def summonPartiallyBuildIterable[Collection: Type]
      : Option[Existential[PartiallyBuildIterableExpr[Collection, *]]] = {
    val inferred = ChimneyType.PartiallyBuildIterable.inferred[Collection]
    import inferred.Underlying as Inferred
    Expr.summonImplicit[Inferred].map { partiallyBuildIterableExpr =>
      val ChimneyType.PartiallyBuildIterable(_, item) = partiallyBuildIterableExpr.tpe: @unchecked
      import item.Underlying as Item
      Existential[PartiallyBuildIterableExpr[Collection, *], Item](
        partiallyBuildIterableExpr
          .asInstanceOf[Expr[io.scalaland.chimney.integrations.PartiallyBuildIterable[Collection, Item]]]
      )
    }
  }

  final protected type TotallyBuildIterableExpr[Collection, Value] =
    Expr[io.scalaland.chimney.integrations.TotallyBuildIterable[Collection, Value]]
  final protected def summonTotallyBuildIterable[Collection: Type]
      : Option[Existential[TotallyBuildIterableExpr[Collection, *]]] = {
    val inferred = ChimneyType.TotallyBuildIterable.inferred[Collection]
    import inferred.Underlying as Inferred
    Expr.summonImplicit[Inferred].map { totallyBuildIterableExpr =>
      val ChimneyType.TotallyBuildIterable(_, item) = totallyBuildIterableExpr.tpe: @unchecked
      import item.Underlying as Item
      Existential[TotallyBuildIterableExpr[Collection, *], Item](
        totallyBuildIterableExpr
          .asInstanceOf[Expr[io.scalaland.chimney.integrations.TotallyBuildIterable[Collection, Item]]]
      )
    }
  }
}
