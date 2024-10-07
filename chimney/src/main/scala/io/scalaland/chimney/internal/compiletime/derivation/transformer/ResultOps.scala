package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{
  AmbiguousFieldOverrides,
  AmbiguousFieldSources,
  AmbiguousImplicitPriority,
  AmbiguousSubtypeTargets,
  DerivationResult,
  MissingConstructorArgument,
  MissingFieldTransformer,
  MissingJavaBeanSetterParam,
  MissingSubtypeTransformer,
  NotSupportedRenameFromPath,
  NotSupportedTransformerDerivation,
  TupleArityMismatch
}
import io.scalaland.chimney.integrations as in
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

private[compiletime] trait ResultOps { this: Derivation =>

  import ChimneyType.Implicits.*

  /** DerivationResult is defined outside the "cake", so methods using utilities from the cake have to be extensions */
  implicit final protected class DerivationResultModule(derivationResult: DerivationResult.type) {

    def existential[F[_], A: Type](fa: F[A]): DerivationResult[Existential[F]] =
      DerivationResult.pure(Existential[F, A](fa))

    def expanded[To](expr: TransformationExpr[To]): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult.pure(Rule.ExpansionResult.Expanded(expr))

    def expandedTotal[To](expr: Expr[To]): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult.pure(Rule.ExpansionResult.Expanded(TransformationExpr.TotalExpr[To](expr)))

    def expandedPartial[To](expr: Expr[partial.Result[To]]): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult.pure(Rule.ExpansionResult.Expanded(TransformationExpr.PartialExpr[To](expr)))

    def attemptNextRule[A]: DerivationResult[Rule.ExpansionResult[A]] =
      DerivationResult.pure(Rule.ExpansionResult.AttemptNextRule(None))
    def attemptNextRuleBecause[A](reason: String): DerivationResult[Rule.ExpansionResult[A]] =
      DerivationResult.pure(Rule.ExpansionResult.AttemptNextRule(Some(reason)))

    def missingConstructorArgument[From, To, Field: Type, A](
        toField: String,
        availableMethodAccessors: List[String],
        availableInheritedAccessors: List[String],
        availableDefault: Boolean,
        availableNone: Boolean
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      MissingConstructorArgument(
        toField = toField,
        toFieldType = Type.prettyPrint[Field],
        availableMethodAccessors = availableMethodAccessors,
        availableInheritedAccessors = availableInheritedAccessors,
        availableDefault = availableDefault,
        availableNone = availableNone
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def missingJavaBeanSetterParam[From, To, Setter: Type, A](
        toSetter: String,
        availableMethodAccessors: List[String],
        availableInheritedAccessors: List[String],
        availableNone: Boolean
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      MissingJavaBeanSetterParam(
        toSetter = toSetter,
        toSetterType = Type.prettyPrint[Setter],
        availableMethodAccessors = availableMethodAccessors,
        availableInheritedAccessors = availableInheritedAccessors,
        availableNone = availableNone
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def missingFieldTransformer[From, To, FromField: Type, ToField: Type, A](toField: String)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      MissingFieldTransformer(
        toField = toField,
        fromFieldType = Type.prettyPrint[FromField],
        toFieldType = Type.prettyPrint[ToField]
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def ambiguousFieldSources[From, To, A](
        foundFromFields: List[String],
        toField: String
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[A] = DerivationResult.transformerError(
      AmbiguousFieldSources(
        foundFromFields = foundFromFields.sorted,
        toField = toField
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def ambiguousFieldOverrides[From, To, A](
        toName: String,
        foundOverrides: List[String],
        fieldNamesComparator: String
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[A] = DerivationResult.transformerError(
      AmbiguousFieldOverrides(
        toName = toName,
        foundOverrides = foundOverrides.sorted,
        fieldNamesComparator = fieldNamesComparator
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def notSupportedRenameFromPath[From, To, A](
        toName: String,
        foundFromPath: Path,
        allowedFromPaths: Path
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[A] = DerivationResult.transformerError(
      NotSupportedRenameFromPath(
        toName = toName,
        foundFromPath = foundFromPath.toString,
        allowedFromPaths = allowedFromPaths.toString
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def missingSubtypeTransformer[From, To, FromSubtype: Type, A](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      MissingSubtypeTransformer(
        fromSubtype = Type.prettyPrint[FromSubtype]
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def ambiguousSubtypeTargets[From, To, A](
        fromSubtype: ExistentialType,
        foundToSubtypes: List[ExistentialType]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[A] = DerivationResult.transformerError(
      AmbiguousSubtypeTargets(
        fromSubtype = {
          import fromSubtype.Underlying as FromSubtype
          Type.prettyPrint[FromSubtype]
        },
        foundToSubtypes = foundToSubtypes.map { foundToSubtype =>
          import foundToSubtype.Underlying as ToSubtype
          Type.prettyPrint[ToSubtype]
        }.sorted
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def tupleArityMismatch[From, To, A](fromArity: Int, toArity: Int)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      TupleArityMismatch(
        fromArity = fromArity,
        toArity = toArity
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def ambiguousImplicitPriority[From, To, A](
        total: Expr[Transformer[From, To]],
        partial: Expr[PartialTransformer[From, To]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      AmbiguousImplicitPriority(
        totalExprPrettyPrint = total.prettyPrint,
        partialExprPrettyPrint = partial.prettyPrint
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def ambiguousImplicitOuterPriority[From, To, InnerFromT: Type, InnerToT: Type, InnerFromP: Type, InnerToP: Type, A](
        total: Expr[in.TotalOuterTransformer[From, To, InnerFromT, InnerToT]],
        partial: Expr[in.PartialOuterTransformer[From, To, InnerFromP, InnerToP]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      AmbiguousImplicitPriority(
        totalExprPrettyPrint = total.prettyPrint,
        partialExprPrettyPrint = partial.prettyPrint
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def notSupportedTransformerDerivation[From, To, A](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      NotSupportedTransformerDerivation(
        exprPrettyPrint = ctx.src.prettyPrint
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )
    def notSupportedTransformerDerivationForField[From, To, A](fieldName: String)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      NotSupportedTransformerDerivation(
        exprPrettyPrint = fieldName
      )(fromType = Type.prettyPrint[From], toType = Type.prettyPrint[To])
    )

    def summonImplicit[A: Type]: DerivationResult[Expr[A]] = DerivationResult(Expr.summonImplicitUnsafe[A])
  }
}
