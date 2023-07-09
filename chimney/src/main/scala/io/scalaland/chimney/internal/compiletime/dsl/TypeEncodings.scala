package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.runtime

private[chimney] trait TypeEncodings { this: DslDefinitions =>

  protected def extractSelectorFieldName[A: Type, Field: Type](
      selector: Expr[A => Field]
  ): Option[ExistentialType.UpperBounded[String]]

  final protected def extractSelectorFieldNameOrFail[A: Type, Field: Type](
      selector: Expr[A => Field]
  ): ExistentialType.UpperBounded[String] = extractSelectorFieldName[A, Field](selector).getOrElse(
    reportError(selectorMessage(selector))
  )

  // This (suppressed) error is a case when Scala 3 compiler is simply wrong :)
  @scala.annotation.nowarn("msg=Unreachable case")
  final protected def extractSelectorFieldNamesOrFail[A: Type, AField: Type, B: Type, BField: Type](
      selectorA: Expr[A => AField],
      selectorB: Expr[B => BField]
  ): (ExistentialType.UpperBounded[String], ExistentialType.UpperBounded[String]) =
    (extractSelectorFieldName[A, AField](selectorA), extractSelectorFieldName[B, BField](selectorB)) match {
      case (None, None) =>
        reportError(s"Invalid selectors:\n${selectorMessage(selectorA)}\n${selectorMessage(selectorB)}")
      case (None, _) =>
        reportError(selectorMessage(selectorA))
      case (_, None) =>
        reportError(selectorMessage(selectorB))
      case (Some(fieldA), Some(fieldB)) =>
        fieldA -> fieldB
    }

  private def selectorMessage[A](expr: Expr[A]) = s"Invalid selector expression: ${Expr.prettyPrint(expr)}"

  implicit protected class DefinitionsOps[F[
      _,
      _,
      _ <: runtime.TransformerCfg,
      _ <: runtime.TransformerFlags
  ], From, To, Cfg <: runtime.TransformerCfg, Flags <: runtime.TransformerFlags](
      private val definition: Expr[F[From, To, Cfg, Flags]]
  ) {

    def updateCfg[NewCfg <: runtime.TransformerCfg](implicit
        oldF: Type[F[From, To, Cfg, Flags]],
        newF: Type[F[From, To, NewCfg, Flags]]
    ): Expr[F[From, To, NewCfg, Flags]] =
      definition.asInstanceOfExpr[F[From, To, NewCfg, Flags]]

    def forgetCfg: Expr[F[From, To, ? <: runtime.TransformerCfg, Flags]] =
      definition.asInstanceOf[Expr[F[From, To, ? <: runtime.TransformerCfg, Flags]]]
  }
}
