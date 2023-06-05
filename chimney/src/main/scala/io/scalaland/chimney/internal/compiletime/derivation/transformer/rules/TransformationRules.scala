package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait TransformationRules { this: Derivation =>

  import ChimneyTypeImplicits.*

  abstract protected class Rule(val name: String) {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]]
  }

  protected object Rule {

    sealed trait ExpansionResult[+A]

    object ExpansionResult {
      // successfully expanded transformation expr
      case class Expanded[A](transformationExpr: TransformationExpr[A]) extends ExpansionResult[A]

      // continue expansion with another rule on the list
      case object Continue extends ExpansionResult[Nothing]
    }

    def expandRules[From, To](
        rules: List[Rule]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[TransformationExpr[To]] = {
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
                  .as(transformationExpr.asInstanceOf[TransformationExpr[To]])
              case ExpansionResult.Continue =>
                DerivationResult.log(s"Rule ${rule.name} decided to continue expansion") >>
                  expandRules[From, To](nextRules)
            }
      }
    }
  }

  sealed protected trait TransformationExpr[A] extends Product with Serializable {

    import TransformationExpr.{PartialExpr, TotalExpr}

    final def map[B: Type](f: Expr[A] => Expr[B]): TransformationExpr[B] = this match {
      case TotalExpr(expr) => TotalExpr(f(expr))
      case PartialExpr(expr) =>
        val ChimneyType.PartialResult(a) = Expr.typeOf(expr): @unchecked
        implicit val A: Type[A] = a.Type.asInstanceOf[Type[A]]
        PartialExpr(ChimneyExpr.PartialResult.map(expr)(Expr.Function1.lift(f)))
    }

    final def fold[B](onTotal: Expr[A] => B)(onPartial: Expr[partial.Result[A]] => B): B = this match {
      case TotalExpr(expr)   => onTotal(expr)
      case PartialExpr(expr) => onPartial(expr)
    }

    final def toEither: Either[Expr[A], Expr[partial.Result[A]]] =
      fold[Either[Expr[A], Expr[partial.Result[A]]]](e => Left(e))(e => Right(e))

    final def ensureTotal: Expr[A] = fold(identity) { _ =>
      assertionFailed("Derived partial.Result expression where total Transformer expects direct value")
    }

    final def ensurePartial: Expr[partial.Result[A]] = fold { expr =>
      implicit val A: Type[A] = Expr.typeOf(expr)
      ChimneyExpr.PartialResult.Value(expr).upcastExpr[partial.Result[A]]
    }(identity)
  }

  protected object TransformationExpr {
    def total[A](expr: Expr[A]): TransformationExpr[A] = TotalExpr(expr)
    def partial[A](expr: Expr[io.scalaland.chimney.partial.Result[A]]): TransformationExpr[A] = PartialExpr(expr)

    final case class TotalExpr[A](expr: Expr[A]) extends TransformationExpr[A]
    final case class PartialExpr[A](expr: Expr[io.scalaland.chimney.partial.Result[A]]) extends TransformationExpr[A]
  }

  protected val rulesAvailableForPlatform: List[Rule]
}
