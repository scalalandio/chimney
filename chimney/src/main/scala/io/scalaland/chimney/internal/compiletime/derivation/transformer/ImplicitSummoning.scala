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
    Expr[io.scalaland.chimney.integrations.OptionalValueOf[Optional, Value]]
  final protected def summonOptionalValue[Optional: Type]: Option[Existential[OptionalValueOfExpr[Optional, *]]] = {
    val inferred = ChimneyType.OptionalValueOf.inferred[Optional]
    import inferred.Underlying as Inferred
    Expr.summonImplicit[Inferred].map { optionalExpr =>
      val ChimneyType.OptionalValueOf(_, value) = optionalExpr.tpe: @unchecked
      import value.Underlying as Value
      Existential[OptionalValueOfExpr[Optional, *], Value](
        optionalExpr.asInstanceOf[Expr[io.scalaland.chimney.integrations.OptionalValueOf[Optional, Value]]]
      )
    }
  }

  final protected type PartiallyBuildIterableOfExpr[Collection, Value] =
    Expr[io.scalaland.chimney.integrations.PartiallyBuildIterableOf[Collection, Value]]
  final protected def summonPartiallyBuildIterable[Collection: Type]
      : Option[Existential[PartiallyBuildIterableOfExpr[Collection, *]]] = {
    val inferred = ChimneyType.PartiallyBuildIterableOf.inferred[Collection]
    import inferred.Underlying as Inferred
    Expr.summonImplicit[Inferred].map { partiallyBuildIterableExpr =>
      val ChimneyType.PartiallyBuildIterableOf(_, item) = partiallyBuildIterableExpr.tpe: @unchecked
      import item.Underlying as Item
      Existential[PartiallyBuildIterableOfExpr[Collection, *], Item](
        partiallyBuildIterableExpr
          .asInstanceOf[Expr[io.scalaland.chimney.integrations.PartiallyBuildIterableOf[Collection, Item]]]
      )
    }
  }

  final protected type TotallyBuildIterableOfExpr[Collection, Value] =
    Expr[io.scalaland.chimney.integrations.TotallyBuildIterableOf[Collection, Value]]
  final protected def summonTotallyBuildIterable[Collection: Type]
      : Option[Existential[TotallyBuildIterableOfExpr[Collection, *]]] = {
    val inferred = ChimneyType.TotallyBuildIterableOf.inferred[Collection]
    import inferred.Underlying as Inferred
    Expr.summonImplicit[Inferred].map { totallyBuildIterableExpr =>
      val ChimneyType.TotallyBuildIterableOf(_, item) = totallyBuildIterableExpr.tpe: @unchecked
      import item.Underlying as Item
      Existential[TotallyBuildIterableOfExpr[Collection, *], Item](
        totallyBuildIterableExpr
          .asInstanceOf[Expr[io.scalaland.chimney.integrations.TotallyBuildIterableOf[Collection, Item]]]
      )
    }
  }
}
