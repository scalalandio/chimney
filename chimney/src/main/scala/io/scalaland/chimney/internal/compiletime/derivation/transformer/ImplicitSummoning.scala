package io.scalaland.chimney.internal.compiletime.derivation.transformer

/** Hearth-based port of the pre-Hearth
  * `io.scalaland.chimney.internal.compiletime.derivation.transformer.ImplicitSummoning`.
  *
  * Differences vs the old version:
  *   - `summonTransformerUnchecked`/`summonPartialTransformerUnchecked` were abstract here and implemented TWICE (in
  *     the Scala 2 and Scala 3 `DerivationPlatform`s, with identical logic modulo quasiquotes vs quotes); Hearth's
  *     `Expr.summonImplicitIgnoring` (the platform-independent version of Scala 3's `summonIgnoring`) lets them be
  *     implemented ONCE, here, in shared code - including the `Iso`/`Codec` fallbacks,
  *   - `Expr.summonImplicit[A]: Option[...]` call sites go through [[MacroCommonsCompat.summonImplicitOptionOf]]
  *     (Hearth's `summonImplicit` returns `SummoningResult`).
  */
private[compiletime] trait ImplicitSummoning { this: Derivation & hearth.MacroCommons =>

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

  private def ignoredCompanionImplicits[Companion: Type](names: String*): List[UntypedMethod] = {
    val wanted = names.toSet
    Type[Companion].methods.collect { case method if wanted(method.name) => method.asUntyped }
  }

  private lazy val ignoredTransformerImplicits: List[UntypedMethod] = {
    implicit val TransformerModule: Type[io.scalaland.chimney.Transformer.type] =
      Type.of[io.scalaland.chimney.Transformer.type]
    ignoredCompanionImplicits[io.scalaland.chimney.Transformer.type](
      "derive", // handled by recursion in macro
      "transformerFromIsoFirst", // handled below
      "transformerFromIsoSecond", // handled below
      "transformerFromCodecEncoder" // handled below
    )
  }

  protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    Expr
      .summonImplicitIgnoring[io.scalaland.chimney.Transformer[From, To]](ignoredTransformerImplicits*)
      .toOption
      .orElse(summonTransformerFromIsoFirst[From, To])
      .orElse(summonTransformerFromIsoSecond[From, To])
      .orElse(summonTransformerFromCodecEncoder[From, To])

  private def summonTransformerFromIsoFirst[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] = {
    implicit val IsoType: Type[io.scalaland.chimney.Iso[From, To]] = Type.of[io.scalaland.chimney.Iso[From, To]]
    summonImplicitOptionOf[io.scalaland.chimney.Iso[From, To]].map(iso => Expr.quote(Expr.splice(iso).first))
  }

  private def summonTransformerFromIsoSecond[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] = {
    implicit val IsoType: Type[io.scalaland.chimney.Iso[To, From]] = Type.of[io.scalaland.chimney.Iso[To, From]]
    summonImplicitOptionOf[io.scalaland.chimney.Iso[To, From]].map(iso => Expr.quote(Expr.splice(iso).second))
  }

  private def summonTransformerFromCodecEncoder[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] = {
    implicit val CodecType: Type[io.scalaland.chimney.Codec[From, To]] = Type.of[io.scalaland.chimney.Codec[From, To]]
    summonImplicitOptionOf[io.scalaland.chimney.Codec[From, To]].map(codec => Expr.quote(Expr.splice(codec).encode))
  }

  private lazy val ignoredPartialTransformerImplicits: List[UntypedMethod] = {
    implicit val PartialTransformerModule: Type[io.scalaland.chimney.PartialTransformer.type] =
      Type.of[io.scalaland.chimney.PartialTransformer.type]
    ignoredCompanionImplicits[io.scalaland.chimney.PartialTransformer.type](
      "derive", // handled by recursion in macro
      "partialTransformerFromCodecDecoder" // handled below
    )
  }

  protected def summonPartialTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    Expr
      .summonImplicitIgnoring[io.scalaland.chimney.PartialTransformer[From, To]](ignoredPartialTransformerImplicits*)
      .toOption
      .orElse(summonPartialTransformerFromCodecDecoder[From, To])

  private def summonPartialTransformerFromCodecDecoder[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] = {
    implicit val CodecType: Type[io.scalaland.chimney.Codec[To, From]] = Type.of[io.scalaland.chimney.Codec[To, From]]
    summonImplicitOptionOf[io.scalaland.chimney.Codec[To, From]].map(codec => Expr.quote(Expr.splice(codec).decode))
  }

  final protected def summonTotalOuterTransformer[From: Type, To: Type]: Option[TotalOuterTransformer[From, To]] = {
    val inferred = ChimneyType.TotalOuterTransformer.inferred[From, To]
    import inferred.Underlying as Inferred
    summonImplicitOptionOf[Inferred].map { outerTransformerExpr =>
      val ChimneyType.TotalOuterTransformer(_, _, innerFrom, innerTo) = outerTransformerExpr.tpe: @unchecked
      new TotalOuterTransformer[From, To] {
        type InnerFrom = innerFrom.Underlying
        implicit val InnerFrom: Type[InnerFrom] = innerFrom.Underlying

        type InnerTo = innerTo.Underlying
        implicit val InnerTo: Type[InnerTo] = innerTo.Underlying

        val instance: Expr[io.scalaland.chimney.integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]] =
          outerTransformerExpr
            .asInstanceOf[Expr[io.scalaland.chimney.integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]]]
      }
    }
  }

  final protected def summonPartialOuterTransformer[From: Type, To: Type]: Option[PartialOuterTransformer[From, To]] = {
    val inferred = ChimneyType.PartialOuterTransformer.inferred[From, To]
    import inferred.Underlying as Inferred
    summonImplicitOptionOf[Inferred].map { outerTransformerExpr =>
      val ChimneyType.PartialOuterTransformer(_, _, innerFrom, innerTo) = outerTransformerExpr.tpe: @unchecked
      new PartialOuterTransformer[From, To] {
        type InnerFrom = innerFrom.Underlying
        implicit val InnerFrom: Type[InnerFrom] = innerFrom.Underlying

        type InnerTo = innerTo.Underlying
        implicit val InnerTo: Type[InnerTo] = innerTo.Underlying

        val instance: Expr[io.scalaland.chimney.integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]] =
          outerTransformerExpr
            .asInstanceOf[Expr[io.scalaland.chimney.integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]]]
      }
    }
  }

  final protected def summonDefaultValue[Value: Type]
      : Option[Expr[io.scalaland.chimney.integrations.DefaultValue[Value]]] =
    summonImplicitOptionOf[io.scalaland.chimney.integrations.DefaultValue[Value]]

  final protected type OptionalValueExpr[Optional, Value] =
    Expr[io.scalaland.chimney.integrations.OptionalValue[Optional, Value]]
  final protected def summonOptionalValue[Optional: Type]: Option[Existential[OptionalValueExpr[Optional, *]]] = {
    val inferred = ChimneyType.OptionalValue.inferred[Optional]
    import inferred.Underlying as Inferred
    summonImplicitOptionOf[Inferred].map { optionalExpr =>
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
    summonImplicitOptionOf[Inferred].map { partiallyBuildIterableExpr =>
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
    summonImplicitOptionOf[Inferred].map { totallyBuildIterableExpr =>
      val ChimneyType.TotallyBuildIterable(_, item) = totallyBuildIterableExpr.tpe: @unchecked
      import item.Underlying as Item
      Existential[TotallyBuildIterableExpr[Collection, *], Item](
        totallyBuildIterableExpr
          .asInstanceOf[Expr[io.scalaland.chimney.integrations.TotallyBuildIterable[Collection, Item]]]
      )
    }
  }
}
