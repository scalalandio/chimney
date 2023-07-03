package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.{
  AmbiguousCoproductInstance,
  CantFindCoproductInstanceTransformer,
  CantFindValueClassMember,
  IncompatibleSourceTuple,
  MissingAccessor,
  MissingJavaBeanSetterParam,
  MissingTransformer,
  NotSupportedTransformerDerivation
}
import io.scalaland.chimney.partial

private[compiletime] trait ResultOps { this: Derivation =>

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
      DerivationResult.pure(Rule.ExpansionResult.AttemptNextRule)

    def missingAccessor[From, To, Field: Type, A](fieldName: String, isAccessorAvailable: Boolean)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      MissingAccessor(
        fieldName = fieldName,
        fieldTypeName = Type.prettyPrint[Field],
        sourceTypeName = Type.prettyPrint[From],
        targetTypeName = Type.prettyPrint[To],
        defAvailable = isAccessorAvailable
      )
    )

    def missingJavaBeanSetterParam[From, To, Setter: Type, A](setterName: String, isAccessorAvailable: Boolean)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      MissingJavaBeanSetterParam(
        setterName = setterName,
        requiredTypeName = Type.prettyPrint[Setter],
        sourceTypeName = Type.prettyPrint[From],
        targetTypeName = Type.prettyPrint[To],
        defAvailable = isAccessorAvailable
      )
    )

    def missingTransformer[From, To, SourceField: Type, TargetField: Type, A](fieldName: String)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      MissingTransformer(
        fieldName = fieldName,
        sourceFieldTypeName = Type.prettyPrint[SourceField],
        targetFieldTypeName = Type.prettyPrint[TargetField],
        sourceTypeName = Type.prettyPrint[From],
        targetTypeName = Type.prettyPrint[To]
      )
    )

    def cantFindValueClassMember[From, To, A](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      CantFindValueClassMember(
        sourceTypeName = Type.prettyPrint[From],
        targetTypeName = Type.prettyPrint[To]
      )
    )

    def cantFindCoproductInstanceTransformer[From, To, Instance: Type, A](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      CantFindCoproductInstanceTransformer(
        instance = Type.prettyPrint[Instance],
        sourceTypeName = Type.prettyPrint[From],
        targetTypeName = Type.prettyPrint[To]
      )
    )

    def ambiguousCoproductInstance[From, To, A](ambiguousName: String)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      AmbiguousCoproductInstance(
        instance = ambiguousName,
        sourceTypeName = Type.prettyPrint[From],
        targetTypeName = Type.prettyPrint[To]
      )
    )

    def incompatibleSourceTuple[From, To, A](sourceArity: Int, targetArity: Int)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      IncompatibleSourceTuple(
        sourceArity = sourceArity,
        targetArity = targetArity,
        sourceTypeName = Type.prettyPrint[From],
        targetTypeName = Type.prettyPrint[To]
      )
    )

    def notSupportedTransformerDerivation[From, To, A](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      NotSupportedTransformerDerivation(
        exprPrettyPrint = ctx.src.prettyPrint,
        sourceTypeName = Type.prettyPrint[From],
        targetTypeName = Type.prettyPrint[To]
      )
    )
    def notSupportedTransformerDerivationForField[From, To, A](fieldName: String)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[A] = DerivationResult.transformerError(
      NotSupportedTransformerDerivation(
        exprPrettyPrint = fieldName,
        sourceTypeName = Type.prettyPrint[From],
        targetTypeName = Type.prettyPrint[To]
      )
    )

    def summonImplicit[A: Type]: DerivationResult[Expr[A]] = Expr
      .summonImplicit[A]
      .fold(
        // TODO: create separate type for missing implicit
        DerivationResult.assertionError[Expr[A]](s"Implicit not found: ${Type.prettyPrint[A]}")
      )(DerivationResult.pure[Expr[A]](_))
  }
}
