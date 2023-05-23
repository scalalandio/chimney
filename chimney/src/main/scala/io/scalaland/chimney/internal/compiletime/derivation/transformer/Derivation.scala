package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Derivation extends Definitions with ResultOps with ImplicitSummoning {

  /** Intended use case: recursive derivation */
  final protected def deriveTransformationResultExpr[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[DerivedExpr[To]] =
    DerivationResult.namedScope(
      ctx match {
        case _: TransformerContext.ForTotal[?, ?] =>
          s"Deriving Total Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"
        case _: TransformerContext.ForPartial[?, ?] =>
          s"Deriving Partial Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"
      }
    ) {
      Rule.expandRules[From, To](rulesAvailableForPlatform)
    }

  final protected def deriveRecursiveTransformationExpr[NewFrom: Type, NewTo: Type](
      newSrc: Expr[NewFrom]
  )(implicit ctx: TransformerContext[?, ?]): DerivationResult[DerivedExpr[NewTo]] = {
    val newCtx: TransformerContext[NewFrom, NewTo] = ctx.updateFromTo[NewFrom, NewTo](newSrc).updateConfig {
      _.prepareForRecursiveCall
    }
    deriveTransformationResultExpr(newCtx)
      .logSuccess {
        case DerivedExpr.TotalExpr(expr)   => s"Derived recursively total expression ${Expr.prettyPrint(expr)}"
        case DerivedExpr.PartialExpr(expr) => s"Derived recursively partial expression ${Expr.prettyPrint(expr)}"
      }
  }

  // proposed initial version of the API, we might make it more generic later on
  // idea:
  //   1. we derive the Expr[To] OR Expr[partial.Result[To]] because we don't know what we'll get until we try
  //   2. THEN we create the code which will initialize Expr[From] because that might depend on the result of derivation
  //   3. we also propagate the errors and Rule.ExpansionResult
  protected def deriveFold[NewFrom: Type, NewTo: Type, To: Type](
      deriveFromSrc: Expr[NewFrom] => DerivationResult[Rule.ExpansionResult[NewTo]]
  )(
      provideSrcForTotal: (Expr[NewFrom] => Expr[NewTo]) => DerivedExpr[To]
  )(
      provideSrcForPartial: (Expr[NewFrom] => Expr[partial.Result[NewTo]]) => DerivedExpr[To]
  ): DerivationResult[Rule.ExpansionResult[To]]

  final protected class DeferredExprInit[From, A](value: A, typedName: DeferredExprInit.TypedName[From]) {

    def map[B](f: A => B): DeferredExprInit[From, B] = new DeferredExprInit(f(value), typedName)

    def initializeAsVal[From2: Type](init: Expr[From2]): DerivationResult[DeferredExprInit.InitializedAsVal[A]] = {
      if (typedName.tpe == Type[From2])
        DerivationResult.pure(DeferredExprInit.InitializedAsVal(value, typedName, init.asInstanceOf[Expr[From]]))
      else
        DerivationResult.fromException(
          new AssertionError(
            s"Initialized deferred Expr[${Type.prettyPrint(typedName.tpe)}] with expression of type ${Type.prettyPrint[From2]}"
          )
        )
    }

    def initializeAsLambdaParam: DeferredExprInit.InitializedAsParam[From, A] =
      new DeferredExprInit.InitializedAsParam(value, typedName)
  }
  protected val DeferredExprInit: DeferredExprInitModule
  protected trait DeferredExprInitModule { this: DeferredExprInit.type =>

    final case class TypedName[T](tpe: Type[T], name: String)

    def use[From: Type, To: Type](
        f: Expr[From] => DerivationResult[DerivedExpr[To]]
    ): DerivationResult[DeferredExprInit[From, DerivedExpr[To]]] = {
      val typedName = TypedName(Type[From], provideFreshName())
      f(refToTypedName(typedName)).map(new DeferredExprInit(_, typedName))
    }

    final class InitializedAsVal[A] private (private val value: A, private val inits: Vector[InitializedAsVal.Init]) {

      def map[B](f: A => B): InitializedAsVal[B] = new InitializedAsVal(f(value), inits)

      def map2[B, C](init2: InitializedAsVal[B])(f: (A, B) => C): InitializedAsVal[C] =
        new InitializedAsVal(f(value, init2.value), inits ++ init2.inits)

      def asInitializedExpr[B](implicit ev: A <:< DerivedExpr[B]): DerivedExpr[B] = initializeVals(this.map(ev))
    }
    object InitializedAsVal {
      def apply[A, From](value: A, typedName: TypedName[From], initExpr: Expr[From]): InitializedAsVal[A] =
        new InitializedAsVal(
          value,
          Vector(
            new Init {
              type T = From
              val name = typedName
              val init = initExpr
            }
          )
        )

      private trait Init {
        type T
        val name: TypedName[T]
        val init: Expr[T]
      }
    }

    final class InitializedAsParam[From, A](value: A, typedName: TypedName[From]) {

      def map[B](f: A => B): InitializedAsParam[From, B] = new InitializedAsParam(f(value), typedName)

      def injectLambda[To: Type, B](f: Expr[From => To] => B)(implicit ev: A <:< Expr[To]): B =
        useLambda(this.map(ev), f)
    }

    protected def provideFreshName(): String
    protected def refToTypedName[T](typedName: TypedName[T]): Expr[T]
    protected def initializeVals[To](init: InitializedAsVal[DerivedExpr[To]]): DerivedExpr[To]
    protected def useLambda[From, To, B](param: InitializedAsParam[From, Expr[To]], usage: Expr[From => To] => B): B
  }

  abstract protected class Rule(val name: String) {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]]
  }

  protected object Rule {

    sealed trait ExpansionResult[+A]

    object ExpansionResult {
      // successfully expanded transformation expr
      case class Expanded[A](transformationExpr: DerivedExpr[A]) extends ExpansionResult[A]
      // continue expansion with another rule on the list
      case object Continue extends ExpansionResult[Nothing]
    }

    def expandRules[From, To](
        rules: List[Rule]
    )(implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] = {
      rules match {
        case Nil =>
          DerivationResult.notSupportedTransformerDerivation
        case rule :: nextRules =>
          DerivationResult
            .namedScope(s"Attempting expansion of rule ${rule.name}")(
              rule.expand[From, To].logFailure { errors => errors.prettyPrint }
            )
            .flatMap {
              case ExpansionResult.Expanded(transformationExpr) =>
                DerivationResult
                  .log(s"Rule ${rule.name} expanded successfully")
                  .as(transformationExpr.asInstanceOf[DerivedExpr[To]])
              case ExpansionResult.Continue =>
                DerivationResult.log(s"Rule ${rule.name} decided to continue expansion") >>
                  expandRules[From, To](nextRules)
            }
      }
    }
  }

  sealed protected trait DerivedExpr[A] // TODO: rename to TransformationExpr

  protected object DerivedExpr {
    final case class TotalExpr[A](expr: Expr[A]) extends DerivedExpr[A]
    final case class PartialExpr[A](expr: Expr[partial.Result[A]]) extends DerivedExpr[A]
  }

  protected val rulesAvailableForPlatform: List[Rule]
}
