package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.partial

private[compiletime] trait Contexts { this: Derivation =>

  /** Stores all the "global" information that might be needed: types used, user configuration, runtime values, etc */
  sealed protected trait TransformationContext[From, To] extends scala.Product with Serializable {
    val src: Expr[From]

    val From: Type[From]
    val To: Type[To]

    val srcJournal: Vector[(Path, ExistentialExpr)]
    val tgtJournal: Vector[Path]

    /** When using nested paths (_.foo.bar.baz) and recursive derivation this is the original, "top-level" value */
    val originalSrc: ExistentialExpr = srcJournal.head._2
    val currentSrc: Path = srcJournal.last._1

    /** Path to the current target value */
    val currentTgt: Path = tgtJournal.last

    val config: TransformerConfiguration
    val derivationStartedAt: java.time.Instant

    type Target
    val Target: Type[Target]

    def sourceFieldsUsedByOverrides: List[String] = config.sourceFieldsUsedByOverrides(currentSrc)(this)
    def targetSubtypesUsedByOverrides: List[ExistentialType] = config.targetSubtypesUsedByOverrides(currentTgt)(this)

    def updateFromTo[NewFrom: Type, NewTo: Type](
        newSrc: Expr[NewFrom],
        followFrom: Path = Path.Root,
        followTo: Path = Path.Root
    ): TransformationContext[NewFrom, NewTo] =
      fold[TransformationContext[NewFrom, NewTo]] { (ctx: TransformationContext.ForTotal[From, To]) =>
        TransformationContext.ForTotal[NewFrom, NewTo](src = newSrc)(
          From = Type[NewFrom],
          To = Type[NewTo],
          srcJournal = ctx.srcJournal :+ (ctx.srcJournal.last._1.concat(followFrom) -> newSrc.as_??),
          tgtJournal = ctx.tgtJournal :+ ctx.tgtJournal.last.concat(followTo),
          config = ctx.config.prepareForRecursiveCall(followFrom, followTo)(ctx),
          ctx.derivationStartedAt
        )
      } { (ctx: TransformationContext.ForPartial[From, To]) =>
        TransformationContext.ForPartial[NewFrom, NewTo](src = newSrc, failFast = ctx.failFast)(
          From = Type[NewFrom],
          To = Type[NewTo],
          srcJournal = ctx.srcJournal :+ (ctx.srcJournal.last._1.concat(followFrom) -> newSrc.as_??),
          tgtJournal = ctx.tgtJournal :+ ctx.tgtJournal.last.concat(followTo),
          config = ctx.config.prepareForRecursiveCall(followFrom, followTo)(ctx),
          ctx.derivationStartedAt
        )
      }

    def updateConfig(update: TransformerConfiguration => TransformerConfiguration): this.type =
      fold[TransformationContext[From, To]] { (ctx: TransformationContext.ForTotal[From, To]) =>
        TransformationContext.ForTotal[From, To](src = ctx.src)(
          From = ctx.From,
          To = ctx.To,
          srcJournal = ctx.srcJournal,
          tgtJournal = ctx.tgtJournal,
          config = update(ctx.config),
          derivationStartedAt = ctx.derivationStartedAt
        )
      } { (ctx: TransformationContext.ForPartial[From, To]) =>
        TransformationContext.ForPartial[From, To](src = ctx.src, failFast = ctx.failFast)(
          From = ctx.From,
          To = ctx.To,
          srcJournal = ctx.srcJournal,
          tgtJournal = ctx.tgtJournal,
          config = update(ctx.config),
          derivationStartedAt = ctx.derivationStartedAt
        )
      }.asInstanceOf[this.type]

    /** Avoid clumsy
      * {{{
      *
      * ctx match {
      *   case total: TransformationContext.ForTotal[?, ?]     => ...
      *   case partial: TransformationContext.ForPartial[?, ?] => ...
      * }
      * }}}
      */
    def fold[B](
        forTotal: TransformationContext.ForTotal[From, To] => B
    )(
        forPartial: TransformationContext.ForPartial[From, To] => B
    ): B
  }

  protected object TransformationContext {

    final case class ForTotal[From, To](src: Expr[From])(
        val From: Type[From],
        val To: Type[To],
        val srcJournal: Vector[(Path, ExistentialExpr)],
        val tgtJournal: Vector[Path],
        val config: TransformerConfiguration,
        val derivationStartedAt: java.time.Instant
    ) extends TransformationContext[From, To] {

      final type Target = To
      val Target = To

      override def fold[B](
          forTotal: TransformationContext.ForTotal[From, To] => B
      )(
          forPartial: TransformationContext.ForPartial[From, To] => B
      ): B = forTotal(this)

      override def toString: String =
        s"ForTotal[From = ${Type.prettyPrint(From)}, To = ${Type.prettyPrint(To)}](src = ${Expr.prettyPrint(src)})($config)"
    }

    object ForTotal {

      def create[From: Type, To: Type](
          src: Expr[From],
          config: TransformerConfiguration
      ): ForTotal[From, To] =
        ForTotal(src = src)(
          From = Type[From],
          To = Type[To],
          srcJournal = Vector(Path.Root -> src.as_??),
          tgtJournal = Vector(Path.Root),
          config = config.preventImplicitSummoningFor[From, To],
          derivationStartedAt = java.time.Instant.now()
        )
    }

    final case class ForPartial[From, To](src: Expr[From], failFast: Expr[Boolean])(
        val From: Type[From],
        val To: Type[To],
        val srcJournal: Vector[(Path, ExistentialExpr)],
        val tgtJournal: Vector[Path],
        val config: TransformerConfiguration,
        val derivationStartedAt: java.time.Instant
    ) extends TransformationContext[From, To] {

      final type Target = partial.Result[To]
      val Target = ChimneyType.PartialResult(To)

      override def fold[B](
          forTotal: TransformationContext.ForTotal[From, To] => B
      )(
          forPartial: TransformationContext.ForPartial[From, To] => B
      ): B = forPartial(this)

      override def toString: String =
        s"ForPartial[From = ${Type.prettyPrint(From)}, To = ${Type
            .prettyPrint(To)}](src = ${Expr.prettyPrint(src)}, failFast = ${Expr.prettyPrint(failFast)})($config)"
    }

    object ForPartial {

      def create[From: Type, To: Type](
          src: Expr[From],
          failFast: Expr[Boolean],
          config: TransformerConfiguration
      ): ForPartial[From, To] = ForPartial(src = src, failFast = failFast)(
        From = Type[From],
        To = Type[To],
        srcJournal = Vector(Path.Root -> src.as_??),
        tgtJournal = Vector(Path.Root),
        config = config.preventImplicitSummoningFor[From, To],
        derivationStartedAt = java.time.Instant.now()
      )
    }
  }

  // unpacks Types from Contexts
  implicit final protected def ctx2FromType[From, To](implicit ctx: TransformationContext[From, To]): Type[From] =
    ctx.From
  implicit final protected def ctx2ToType[From, To](implicit ctx: TransformationContext[From, To]): Type[To] = ctx.To

  implicit def areFieldNamesMatching(fromName: String, toName: String)(implicit
      ctx: TransformationContext[?, ?]
  ): Boolean = ctx.config.flags.getFieldNameComparison.namesMatch(fromName, toName)
  implicit def areSubtypeNamesMatching(fromName: String, toName: String)(implicit
      ctx: TransformationContext[?, ?]
  ): Boolean = ctx.config.flags.getSubtypeNameComparison.namesMatch(fromName, toName)

  // for unpacking Exprs from Context, pattern matching should be enough
}
