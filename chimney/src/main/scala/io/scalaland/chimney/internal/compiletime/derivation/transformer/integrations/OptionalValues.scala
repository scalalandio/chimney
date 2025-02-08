package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait OptionalValues { this: Derivation =>

  import Type.Implicits.*

  /** Something allowing us to share the logic which handles [[scala.Option]], [[java.util.Optional]] and whatever we
    * want to support.
    *
    * Tries to use [[io.scalaland.chimney.integrations.OptionalValue]] and then falls back on [[scala.Option]] hardcoded
    * support, if type is eligible.
    */
  abstract protected class OptionalValue[Optional, Value] {
    def empty: Expr[Optional]

    def of(value: Expr[Value]): Expr[Optional]

    def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A]

    def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value]

    def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional]
  }
  protected object OptionalValue {

    private type Cached[A] = Option[Existential[OptionalValue[A, *]]]
    private val optionalCache = new Type.Cache[Cached]
    def unapply[Optional](implicit Optional: Type[Optional]): Option[Existential[OptionalValue[Optional, *]]] =
      optionalCache(Optional)(providedSupport[Optional].orElse(buildInOptionSupport[Optional]))

    private def providedSupport[Optional: Type]: Option[Existential[OptionalValue[Optional, *]]] =
      summonOptionalValue[Optional].map { optionalValue =>
        import optionalValue.{Underlying as Value, value as optionalValueExpr}
        Existential[OptionalValue[Optional, *], Value](
          new OptionalValue[Optional, Value] {
            def empty: Expr[Optional] = optionalValueExpr.empty

            def of(value: Expr[Value]): Expr[Optional] = optionalValueExpr.of(value)

            def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A] =
              optionalValueExpr.fold(optional, onNone, onSome)

            def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value] =
              optionalValueExpr.getOrElse(optional, onNone)

            def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional] =
              optionalValueExpr.orElse(optional, optional2)

            override def toString: String = s"support provided by ${Expr.prettyPrint(optionalValueExpr)}"
          }
        )
      }

    private def buildInOptionSupport[Optional: Type]: Option[Existential[OptionalValue[Optional, *]]] =
      Type.Option.unapply(Type[Optional]).map { value =>
        import value.Underlying as Value
        Existential[OptionalValue[Optional, *], Value](
          new OptionalValue[Optional, Value] {
            def empty: Expr[Optional] = Expr.Option.empty[Value].upcastToExprOf[Optional]

            def of(value: Expr[Value]): Expr[Optional] = Expr.Option(value).upcastToExprOf[Optional]

            def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A] =
              Expr.Option.fold(optional.upcastToExprOf[Option[Value]])(onNone)(onSome)

            def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value] =
              Expr.Option.getOrElse(optional.upcastToExprOf[Option[Value]])(onNone)

            def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional] =
              Expr.Option
                .orElse(optional.upcastToExprOf[Option[Value]], optional2.upcastToExprOf[Option[Value]])
                .upcastToExprOf[Optional]

            override def toString: String = s"support build-in for Option-type ${Type.prettyPrint[Optional]}"
          }
        )
      }
  }
}
