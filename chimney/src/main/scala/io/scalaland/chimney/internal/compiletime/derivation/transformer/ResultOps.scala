package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{
  AmbiguousFieldOverrides,
  AmbiguousFieldSources,
  AmbiguousImplicitPriority,
  AmbiguousSubtypeTargets,
  DerivationResult,
  FailedPolicyCheck,
  MissingConstructorArgument,
  MissingFieldTransformer,
  MissingJavaBeanSetterParam,
  MissingSubtypeTransformer,
  NotSupportedOperationFromPath,
  NotSupportedTransformerDerivation,
  TupleArityMismatch
}
import io.scalaland.chimney.integrations as in
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.TransformerDerivationError

private[compiletime] trait ResultOps { this: Derivation =>

  import ChimneyType.Implicits.*

  /** DerivationResult is defined outside the "cake", so methods using utilities from the cake have to be extensions */
  implicit final protected class DerivationResultModule(derivationResult: DerivationResult.type) {

    def existential[F[_], A: Type](fa: F[A]): DerivationResult[Existential[F]] =
      DerivationResult.pure(Existential[F, A](fa))

    def totalExpr[To](expr: Expr[To]): DerivationResult[TransformationExpr[To]] =
      DerivationResult.pure(TransformationExpr.fromTotal(expr))
    def partialExpr[To](expr: Expr[partial.Result[To]]): DerivationResult[TransformationExpr[To]] =
      DerivationResult.pure(TransformationExpr.fromPartial(expr))

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

    def transformerErrorFromCtx[From, To, A](thunk: (String, String, String, String) => TransformerDerivationError)(
        implicit ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      thunk(Type.prettyPrint[From], Type.prettyPrint[To], ctx.srcJournal.last._1.toString, ctx.tgtJournal.last.toString)
    )

    def missingConstructorArgument[From, To, Field: Type, A](
        toField: String,
        availableMethodAccessors: List[String],
        availableInheritedAccessors: List[String],
        availableDefault: Boolean,
        availableNone: Boolean
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = transformerErrorFromCtx(
      MissingConstructorArgument(
        toField = toField,
        toFieldType = Type.prettyPrint[Field],
        availableMethodAccessors = availableMethodAccessors,
        availableInheritedAccessors = availableInheritedAccessors,
        availableDefault = availableDefault,
        availableNone = availableNone
      )
    )

    def missingJavaBeanSetterParam[From, To, Setter: Type, A](
        toSetter: String,
        availableMethodAccessors: List[String],
        availableInheritedAccessors: List[String],
        availableNone: Boolean
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = transformerErrorFromCtx(
      MissingJavaBeanSetterParam(
        toSetter = toSetter,
        toSetterType = Type.prettyPrint[Setter],
        availableMethodAccessors = availableMethodAccessors,
        availableInheritedAccessors = availableInheritedAccessors,
        availableNone = availableNone
      )
    )

    def missingFieldTransformer[From, To, FromField: Type, ToField: Type, A](toField: String)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = transformerErrorFromCtx(
      MissingFieldTransformer(
        toField = toField,
        fromFieldType = Type.prettyPrint[FromField],
        toFieldType = Type.prettyPrint[ToField]
      )
    )

    def ambiguousFieldSources[From, To, A](
        foundFromFields: List[String],
        toField: String
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[A] = transformerErrorFromCtx(
      AmbiguousFieldSources(
        foundFromFields = foundFromFields.sorted,
        toField = toField
      )
    )

    def ambiguousFieldOverrides[From, To, A](
        toName: String,
        foundOverrides: List[String],
        fieldNamesComparator: String
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[A] = transformerErrorFromCtx(
      AmbiguousFieldOverrides(
        toName = toName,
        foundOverrides = foundOverrides.sorted,
        fieldNamesComparator = fieldNamesComparator
      )
    )

    def notSupportedOperationFromPath[From, To, A](
        operation: NotSupportedOperationFromPath.Operation,
        toName: String,
        foundFromPath: Path,
        allowedFromPaths: Path
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[A] = transformerErrorFromCtx(
      NotSupportedOperationFromPath(
        operation = operation,
        toName = toName,
        foundFromPath = foundFromPath.toString,
        allowedFromPaths = allowedFromPaths.toString
      )
    )

    def missingSubtypeTransformer[From, To, FromSubtype: Type, A](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = transformerErrorFromCtx(
      MissingSubtypeTransformer(
        fromSubtype = Type.prettyPrint[FromSubtype]
      )
    )

    def ambiguousSubtypeTargets[From, To, A](
        fromSubtype: ExistentialType,
        foundToSubtypes: List[ExistentialType]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[A] = transformerErrorFromCtx(
      AmbiguousSubtypeTargets(
        fromSubtype = {
          import fromSubtype.Underlying as FromSubtype
          Type.prettyPrint[FromSubtype]
        },
        foundToSubtypes = foundToSubtypes.map { foundToSubtype =>
          import foundToSubtype.Underlying as ToSubtype
          Type.prettyPrint[ToSubtype]
        }.sorted
      )
    )

    def tupleArityMismatch[From, To, A](fromArity: Int, toArity: Int, fallbackArity: List[Int])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = transformerErrorFromCtx(
      TupleArityMismatch(
        fromArity = fromArity,
        toArity = toArity,
        fallbackArity = fallbackArity
      )
    )

    def ambiguousImplicitPriority[From, To, A](
        total: Expr[Transformer[From, To]],
        partial: Expr[PartialTransformer[From, To]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = transformerErrorFromCtx(
      AmbiguousImplicitPriority(
        totalExprPrettyPrint = total.prettyPrint,
        partialExprPrettyPrint = partial.prettyPrint
      )
    )

    def ambiguousImplicitOuterPriority[From, To, InnerFromT: Type, InnerToT: Type, InnerFromP: Type, InnerToP: Type, A](
        total: Expr[in.TotalOuterTransformer[From, To, InnerFromT, InnerToT]],
        partial: Expr[in.PartialOuterTransformer[From, To, InnerFromP, InnerToP]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = transformerErrorFromCtx(
      AmbiguousImplicitPriority(
        totalExprPrettyPrint = total.prettyPrint,
        partialExprPrettyPrint = partial.prettyPrint
      )
    )

    def notSupportedTransformerDerivation[From, To, A](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = transformerErrorFromCtx(
      NotSupportedTransformerDerivation(
        exprPrettyPrint = ctx.src.prettyPrint
      )
    )
    def notSupportedTransformerDerivationForField[From, To, A](fieldName: String)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = transformerErrorFromCtx(
      NotSupportedTransformerDerivation(
        exprPrettyPrint = fieldName
      )
    )

    def failedPolicyCheck[From, To, A](policy: Any, path: Path, failedValues: List[String])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = transformerErrorFromCtx(
      FailedPolicyCheck(
        policyName = policy.toString,
        path = path.toString,
        failedValues = failedValues
      )
    )

    def summonImplicit[A: Type]: DerivationResult[Expr[A]] = DerivationResult(Expr.summonImplicitUnsafe[A])
  }
}
