package io.scalaland.chimney.internal.compiletime.derivation.transformer

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.{
  AmbiguousFieldOverrides,
  AmbiguousFieldSources,
  AmbiguousImplicitPriority,
  AmbiguousSubtypeTargets,
  DerivationError,
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

/** Smart constructors of `MIO` results that need utilities from the cake (`Type`, `Expr`, contexts, ...). */
private[compiletime] trait ResultOps { this: Derivation & hearth.MacroCommons =>

  final protected def existential[F[_], A: Type](fa: F[A]): MIO[Existential[F]] =
    MIO.pure(Existential[F, A](fa))

  final protected def totalExpr[To](expr: Expr[To]): MIO[TransformationExpr[To]] =
    MIO.pure(TransformationExpr.fromTotal(expr))
  final protected def partialExpr[To](expr: Expr[partial.Result[To]]): MIO[TransformationExpr[To]] =
    MIO.pure(TransformationExpr.fromPartial(expr))

  final protected def expanded[To](expr: TransformationExpr[To]): MIO[Rule.ExpansionResult[To]] =
    MIO.pure(Rule.ExpansionResult.Expanded(expr))
  final protected def expandedTotal[To](expr: Expr[To]): MIO[Rule.ExpansionResult[To]] =
    MIO.pure(Rule.ExpansionResult.Expanded(TransformationExpr.TotalExpr[To](expr)))
  final protected def expandedPartial[To](expr: Expr[partial.Result[To]]): MIO[Rule.ExpansionResult[To]] =
    MIO.pure(Rule.ExpansionResult.Expanded(TransformationExpr.PartialExpr[To](expr)))

  final protected def attemptNextRule[A]: MIO[Rule.ExpansionResult[A]] =
    MIO.pure(Rule.ExpansionResult.AttemptNextRule(None))
  final protected def attemptNextRuleBecause[A](reason: String): MIO[Rule.ExpansionResult[A]] =
    MIO.pure(Rule.ExpansionResult.AttemptNextRule(Some(reason)))

  final protected def transformerErrorFromCtx[From, To, A](
      thunk: (String, String, String, String) => TransformerDerivationError
  )(implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = MIO.fail(
    DerivationError.TransformerError(
      thunk(Type.prettyPrint[From], Type.prettyPrint[To], ctx.srcJournal.last._1.toString, ctx.tgtJournal.last.toString)
    )
  )

  final protected def missingConstructorArgument[From, To, Field: Type, A](
      toField: String,
      availableMethodAccessors: List[String],
      availableInheritedAccessors: List[String],
      availableDefault: Boolean,
      availableNone: Boolean
  )(implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = transformerErrorFromCtx(
    MissingConstructorArgument(
      toField = toField,
      toFieldType = Type.prettyPrint[Field],
      availableMethodAccessors = availableMethodAccessors,
      availableInheritedAccessors = availableInheritedAccessors,
      availableDefault = availableDefault,
      availableNone = availableNone
    )
  )

  final protected def missingJavaBeanSetterParam[From, To, Setter: Type, A](
      toSetter: String,
      availableMethodAccessors: List[String],
      availableInheritedAccessors: List[String],
      availableNone: Boolean
  )(implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = transformerErrorFromCtx(
    MissingJavaBeanSetterParam(
      toSetter = toSetter,
      toSetterType = Type.prettyPrint[Setter],
      availableMethodAccessors = availableMethodAccessors,
      availableInheritedAccessors = availableInheritedAccessors,
      availableNone = availableNone
    )
  )

  final protected def missingFieldTransformer[From, To, FromField: Type, ToField: Type, A](toField: String)(implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = transformerErrorFromCtx(
    MissingFieldTransformer(
      toField = toField,
      fromFieldType = Type.prettyPrint[FromField],
      toFieldType = Type.prettyPrint[ToField]
    )
  )

  final protected def ambiguousFieldSources[From, To, A](
      foundFromFields: List[String],
      toField: String
  )(implicit ctx: TransformationContext[From, To]): MIO[A] = transformerErrorFromCtx(
    AmbiguousFieldSources(
      foundFromFields = foundFromFields.sorted,
      toField = toField
    )
  )

  final protected def ambiguousFieldOverrides[From, To, A](
      toName: String,
      foundOverrides: List[String],
      fieldNamesComparator: String
  )(implicit ctx: TransformationContext[From, To]): MIO[A] = transformerErrorFromCtx(
    AmbiguousFieldOverrides(
      toName = toName,
      foundOverrides = foundOverrides.sorted,
      fieldNamesComparator = fieldNamesComparator
    )
  )

  final protected def notSupportedOperationFromPath[From, To, A](
      operation: NotSupportedOperationFromPath.Operation,
      toName: String,
      foundFromPath: Path,
      allowedFromPaths: Path
  )(implicit ctx: TransformationContext[From, To]): MIO[A] = transformerErrorFromCtx(
    NotSupportedOperationFromPath(
      operation = operation,
      toName = toName,
      foundFromPath = foundFromPath.toString,
      allowedFromPaths = allowedFromPaths.toString
    )
  )

  final protected def missingSubtypeTransformer[From, To, FromSubtype: Type, A](implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = transformerErrorFromCtx(
    MissingSubtypeTransformer(
      fromSubtype = Type.prettyPrint[FromSubtype]
    )
  )

  final protected def ambiguousSubtypeTargets[From, To, A](
      fromSubtype: ExistentialType,
      foundToSubtypes: List[ExistentialType]
  )(implicit ctx: TransformationContext[From, To]): MIO[A] = transformerErrorFromCtx(
    AmbiguousSubtypeTargets(
      fromSubtype = {
        import fromSubtype.Underlying as FromSubtype
        Type.prettyPrint[FromSubtype]
      },
      foundToSubtypes = foundToSubtypes
        .map { foundToSubtype =>
          import foundToSubtype.Underlying as ToSubtype
          // Sort by plainPrint (machine-readable, uncolored), display prettyPrint (colored) - Hearth's prettyPrint
          // colors individual name segments, so sorting the colored strings would scramble the order.
          Type.plainPrint[ToSubtype] -> Type.prettyPrint[ToSubtype]
        }
        .sortBy(_._1)
        .map(_._2)
    )
  )

  final protected def tupleArityMismatch[From, To, A](fromArity: Int, toArity: Int, fallbackArity: List[Int])(implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = transformerErrorFromCtx(
    TupleArityMismatch(
      fromArity = fromArity,
      toArity = toArity,
      fallbackArity = fallbackArity
    )
  )

  final protected def ambiguousImplicitPriority[From, To, A](
      total: Expr[Transformer[From, To]],
      partial: Expr[PartialTransformer[From, To]]
  )(implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = transformerErrorFromCtx(
    AmbiguousImplicitPriority(
      totalExprPrettyPrint = total.prettyPrint,
      partialExprPrettyPrint = partial.prettyPrint
    )
  )

  final protected def ambiguousImplicitOuterPriority[
      From,
      To,
      InnerFromT: Type,
      InnerToT: Type,
      InnerFromP: Type,
      InnerToP: Type,
      A
  ](
      total: Expr[in.TotalOuterTransformer[From, To, InnerFromT, InnerToT]],
      partial: Expr[in.PartialOuterTransformer[From, To, InnerFromP, InnerToP]]
  )(implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = transformerErrorFromCtx(
    AmbiguousImplicitPriority(
      totalExprPrettyPrint = total.prettyPrint,
      partialExprPrettyPrint = partial.prettyPrint
    )
  )

  final protected def notSupportedTransformerDerivation[From, To, A](implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = transformerErrorFromCtx(
    NotSupportedTransformerDerivation(
      exprPrettyPrint = ctx.src.prettyPrint
    )
  )
  final protected def notSupportedTransformerDerivationForField[From, To, A](fieldName: String)(implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = transformerErrorFromCtx(
    NotSupportedTransformerDerivation(
      exprPrettyPrint = fieldName
    )
  )

  final protected def failedPolicyCheck[From, To, A](policy: Any, path: Path, failedValues: List[String])(implicit
      ctx: TransformationContext[From, To]
  ): MIO[A] = transformerErrorFromCtx(
    FailedPolicyCheck(
      policyName = policy.toString,
      path = path.toString,
      failedValues = failedValues
    )
  )

  final protected def summonImplicit[A: Type]: MIO[Expr[A]] = MIO(summonImplicitUnsafeOf[A])
}
